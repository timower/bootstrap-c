package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"log"
	"maps"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"sync"
)

// Server represents the language server
type Server struct {
	rootPath     string
	boostrapPath string
	cache        FileCache
	initialized  bool
	symbols      Symbols
}

// FileCache stores the content of opened files for quick access
type FileCache struct {
	mu      sync.RWMutex
	content map[string][]string
}

// Symbols stores declarations and uses per file.
type Symbols struct {
	mu sync.RWMutex

	// Map of symbol ID -> Symbol
	symbolByID map[string]*Symbol

	// Map of sema path -> symbol
	symbolByRootPath map[string][]*Symbol

	// Map from file -> list of symbols
	symbolRefsByPath map[string][]SymbolRef
}

type SymbolRef struct {
	position Position
	id       string
	length   int
}

type Symbol struct {
	file     string
	position Position

	name string
	id   string
}

// GetOrLoadFileContent retrieves file content from cache or loads it from disk
// if not present
func (fc *FileCache) GetOrLoadFileContent(filePath string) ([]string, error) {
	fc.mu.RLock()
	content, ok := fc.content[filePath]
	fc.mu.RUnlock()
	if ok {
		return content, nil
	}
	// Load the file content
	lines, err := readFileLines(filePath)
	if err != nil {
		return nil, err
	}

	fc.UpdateContent(filePath, lines)
	return lines, nil
}

func (fc *FileCache) UpdateContent(filePath string, content []string) {
	fc.mu.Lock()
	fc.content[filePath] = content
	fc.mu.Unlock()
}

func (fc *FileCache) Delete(filePath string) {
	fc.mu.Lock()
	delete(fc.content, filePath)
	fc.mu.Unlock()
}

var version = "self compiled" // Populated with -X main.version

// Main Function
func main() {
	config := parseFlags()

	if config.showVersion {
		fmt.Printf("brio Language Server %s\n", version)
		os.Exit(0)
	}

	log.SetOutput(os.Stderr)

	server := &Server{
		cache: FileCache{
			content: make(map[string][]string),
		},
		boostrapPath: config.brioPath,
		symbols: Symbols{
			symbolByID:       make(map[string]*Symbol),
			symbolByRootPath: make(map[string][]*Symbol),
			symbolRefsByPath: make(map[string][]SymbolRef),
		},
	}

	log.Printf("Starting brio LSP version: %s", version)

	// Main loop to handle LSP messages
	reader := bufio.NewReader(os.Stdin)
	for {
		req, err := readMessage(reader)
		if err != nil {
			sendError(nil, InvalidRequest, "Malformed request", err.Error())
			continue // Ignore malformed request
		}

		// Handle request in a separate goroutine
		go handleRequest(server, req)
	}
}

// readMessage reads a single JSON-RPC message from the reader
func readMessage(reader *bufio.Reader) (RPCRequest, error) {
	contentLength := 0
	for {
		line, err := reader.ReadString('\r')
		if err != nil {
			return RPCRequest{}, fmt.Errorf("error reading header: %v", err)
		}
		b, err := reader.ReadByte()
		if err != nil {
			return RPCRequest{}, fmt.Errorf("error reading header: %v", err)
		}
		if b != '\n' {
			return RPCRequest{}, fmt.Errorf("line endings must be \\r\\n")
		}
		if line == "\r" {
			break // End of headers
		}
		if after, ok := strings.CutPrefix(line, "Content-Length:"); ok {
			clStr := strings.TrimSpace(after)
			cl, err := strconv.Atoi(clStr)
			if err != nil {
				return RPCRequest{}, fmt.Errorf("invalid Content-Length: %v", err)
			}
			contentLength = cl
		}
	}

	body := make([]byte, contentLength)
	_, err := io.ReadFull(reader, body)
	if err != nil {
		return RPCRequest{}, fmt.Errorf("error reading body: %v", err)
	}

	var req RPCRequest
	err = json.Unmarshal(body, &req)
	if err != nil {
		return RPCRequest{}, fmt.Errorf("invalid JSON-RPC request: %v", err)
	}

	return req, nil
}

// Config holds command-line configuration options
type Config struct {
	showVersion   bool
	brioPath string
}

func parseFlags() Config {
	showVersion := flag.Bool("version", false, "Show version info")
	brioPath := flag.String("brio-path", "boostrap", "Path to brio compiler")

	flag.Parse()

	return Config{
		showVersion:   *showVersion,
		brioPath: *brioPath,
	}
}

// checkInitializedOrFail ensures that the server has been successfully initialized.
func checkInitializedOrFail(id json.RawMessage, server *Server, method string) bool {
	// The following methods are allowed even if not initialized:
	// - initialize (the first request)
	// - shutdown and exit (for cleanup)
	if method == "initialize" || method == "shutdown" || method == "exit" {
		return true
	}

	if !server.initialized {
		sendError(id, ServerNotInitialized, "Server not initialized", "Received request before successful initialization")
		return false
	}
	return true
}

// handleRequest routes JSON-RPC requests to appropriate handlers
func handleRequest(server *Server, req RPCRequest) {
	if !checkInitializedOrFail(req.ID, server, req.Method) {
		// Server not initialized and request is not allowed.
		return
	}

	log.Printf("Got request: %s\n", req.Method)
	switch req.Method {
	case "initialize":
		handleInitialize(server, req)
	case "initialized":
		handleInitialized(server, req)
	case "shutdown":
		handleShutdown(server, req)
	case "exit":
		handleExit(server, req)
	case "textDocument/didOpen":
		handleDidOpen(server, req)
	case "textDocument/didChange":
		handleDidChange(server, req)
	case "textDocument/didClose":
		handleDidClose(server, req)
	case "textDocument/didSave":
		handleDidSave(server, req)
	case "textDocument/completion":
		handleCompletion(server, req)
	case "textDocument/definition":
		handleDefinition(server, req)
	case "textDocument/references":
		handleReferences(server, req)
	case "workspace/symbol":
		handleWorkspaceSymbol(server, req)
	case "textDocument/documentSymbol":
		handleDocumentSymbol(server, req)
	case "textDocument/formatting":
		handleFormatting(server, req)
	case "$/cancelRequest":
		handleCancelRequest(server, req)
	case "$/setTrace":
		handleSetTrace(server, req)
	case "$/logTrace":
		handleLogTrace(server, req)
	default:
		// Method not found
		message := fmt.Sprintf("Method not found: %s", req.Method)
		sendError(req.ID, MethodNotFound, message, nil)
	}
}

// handleInitialize processes the 'initialize' request
func handleInitialize(server *Server, req RPCRequest) {
	var params InitializeParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		sendError(req.ID, InvalidParams, "Invalid params", nil)
		return
	}

	if params.RootURI == "" {
		// Use current working directory if RootURI is empty
		cwd, err := os.Getwd()
		if err != nil {
			sendError(req.ID, InternalError, "Failed to get current working directory", err.Error())
			return
		}
		server.rootPath = cwd
	} else {
		// Convert RootURI to filesystem path
		server.rootPath = uriToPath(params.RootURI)
	}

	if err := server.scanWorkspace(); err != nil {
		sendError(req.ID, InternalError, "Internal error while scanning workspace", err.Error())
		return
	}

	// Define server capabilities
	result := InitializeResult{
		Capabilities: ServerCapabilities{
			TextDocumentSync: &TextDocumentSyncOptions{
				Change:    1, // Full synchronization
				OpenClose: true,
				Save:      true,
			},
			DocumentFormattingProvider: true,
			DefinitionProvider:         true,
			ReferencesProvider:         true,
			CompletionProvider: &CompletionOptions{
				TriggerCharacters: []string{".", ">", ":"},
			},
			/*
				WorkspaceSymbolProvider: true,
				DocumentSymbolProvider:  true,
			*/
		},
		Info: ServerInfo{
			Name:    "brio-lsp",
			Version: version,
		},
	}

	sendResult(req.ID, result)
	server.initialized = true
}

// handleInitialized processes the 'initialized' notification
func handleInitialized(_ *Server, _ RPCRequest) {
	// 'initialized' is a notification with no response
}

// handleShutdown processes the 'shutdown' request
func handleShutdown(_ *Server, req RPCRequest) {
	sendResult(req.ID, nil)
}

// handleExit processes the 'exit' notification
func handleExit(_ *Server, _ RPCRequest) {
	os.Exit(0)
}

// handleCancelRequest processes the '$/cancelRequest' notification
// (For canceling in-progress requests)
func handleCancelRequest(_ *Server, _ RPCRequest) {
	// Not currently in use
}

// handleSetTrace() processes the '$/setTrace' notification
// (Controls trace output level)
func handleSetTrace(_ *Server, req RPCRequest) {
	// Not currently in use
	sendResult(req.ID, nil)
}

// handleLogTrace() processes the '$/logTrace' notification
// (For transmitting trace data)
func handleLogTrace(_ *Server, _ RPCRequest) {
	// Not currently in use
}

// handleDidOpen processes the 'textDocument/didOpen' notification
func handleDidOpen(server *Server, req RPCRequest) {
	var params DidOpenTextDocumentParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		return
	}

	uri := params.TextDocument.URI
	content := strings.Split(params.TextDocument.Text, "\n")

	// Cache the opened document's content
	filepath := uriToPath(uri)
	server.cache.UpdateContent(filepath, content)

	semaFile(server, filepath)
}

// handleDidChange processes the 'textDocument/didChange' notification
func handleDidChange(server *Server, req RPCRequest) {
	var params DidChangeTextDocumentParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		return
	}

	uri := params.TextDocument.URI
	filepath := uriToPath(uri)
	if len(params.ContentChanges) > 0 {
		content := strings.Split(params.ContentChanges[0].Text, "\n")
		// Update the cached content
		server.cache.UpdateContent(filepath, content)
	}

	semaFile(server, filepath)
}

// handleDidClose processes the 'textDocument/didClose' notification
func handleDidClose(server *Server, req RPCRequest) {
	var params DidCloseTextDocumentParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		return
	}

	uri := params.TextDocument.URI
	// Remove the document from cache
	server.cache.Delete(uriToPath(uri))
}

// handleDidSave processes the 'textDocument/didSave' notification
func handleDidSave(server *Server, req RPCRequest) {
	var params DidSaveTextDocumentParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		return
	}

	// Get file path from URI
	filepath := uriToPath(params.TextDocument.URI)
	semaFile(server, filepath)
}

// handleCompletion processes the 'textDocument/completion' request
func handleCompletion(server *Server, req RPCRequest) {
	var params CompletionParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		sendError(req.ID, InvalidParams, "Invalid params", nil)
		return
	}

	path := uriToPath(params.TextDocument.URI)
	relPath, err := filepath.Rel(server.rootPath, path)
	if err != nil {
		log.Printf("Error making rel path: %v", err)
		sendError(req.ID, InternalError, "Path Error", nil)
		return
	}

	server.symbols.mu.RLock()
	defer server.symbols.mu.RUnlock()

	items := []CompletionItem{}
	refs, ok := server.symbols.symbolByRootPath[relPath]
	if !ok {
		log.Printf("No items!")
		result := CompletionList{
			IsIncomplete: false,
			Items:        items,
		}

		sendResult(req.ID, result)
		return
	}

	seen := make(map[string]struct{})
	for _, sym := range refs {
		_, ok = seen[sym.name]
		if ok {
			continue
		}

		seen[sym.name] = struct{}{}
		items = append(items, CompletionItem{
			Label: sym.name,
		})
	}

	result := CompletionList{
		IsIncomplete: false,
		Items:        items,
	}

	sendResult(req.ID, result)
}

func findSymbol(symbols *Symbols, relPath string, pos Position) (string, bool) {
	refs, ok := symbols.symbolRefsByPath[relPath]
	if !ok {
		return "", false
	}

	for _, ref := range refs {
		// TODO: make map?
		if ref.position.Line == pos.Line {
			if pos.Character >= ref.position.Character &&
				pos.Character < ref.position.Character+ref.length {
				return ref.id, true
			}
		}
	}
	return "", false
}

// handleDefinition processes the 'textDocument/definition' request
func handleDefinition(server *Server, req RPCRequest) {
	var params TextDocumentPositionParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		sendError(req.ID, InvalidParams, "Invalid params", nil)
		return
	}

	path := uriToPath(params.TextDocument.URI)
	relPath, err := filepath.Rel(server.rootPath, path)
	if err != nil {
		log.Printf("Error making rel path: %v", err)
		sendError(req.ID, InternalError, "Path Error", nil)
		return
	}

	server.symbols.mu.RLock()
	defer server.symbols.mu.RUnlock()

	id, ok := findSymbol(&server.symbols, relPath, params.Position)
	if !ok {
		log.Printf("No symbol not found")
		sendResult(req.ID, nil)
		return
	}

	sym, ok := server.symbols.symbolByID[id]
	if !ok {
		sendResult(req.ID, nil)
		return
	}

	sendResult(req.ID, Location{
		URI: filepathToURI(sym.file),
		Range: Range{
			Start: sym.position,
			End: Position{
				Line:      sym.position.Line,
				Character: sym.position.Character + len(sym.name),
			},
		},
	})
}

func handleReferences(server *Server, req RPCRequest) {
	var params TextDocumentPositionParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		sendError(req.ID, InvalidParams, "Invalid params", nil)
		return
	}

	path := uriToPath(params.TextDocument.URI)
	relPath, err := filepath.Rel(server.rootPath, path)
	if err != nil {
		log.Printf("Error making rel path: %v", err)
		return
	}

	server.symbols.mu.RLock()
	defer server.symbols.mu.RUnlock()

	id, ok := findSymbol(&server.symbols, relPath, params.Position)
	if !ok {
		sendResult(req.ID, nil)
		return
	}

	result := []Location{}

	for path, refs := range server.symbols.symbolRefsByPath {
		for _, ref := range refs {
			if ref.id == id {
				result = append(result, Location{
					URI: filepathToURI(path),
					Range: Range{
						Start: ref.position,
						End: Position{
							Line:      ref.position.Line,
							Character: ref.position.Character + ref.length,
						},
					},
				})
			}
		}
	}

	sendResult(req.ID, result)
}

// handleWorkspaceSymbol processes the 'workspace/symbol' request
func handleWorkspaceSymbol(server *Server, req RPCRequest) {
	var params WorkspaceSymbolParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		sendError(req.ID, InvalidParams, "Invalid params", nil)
		return
	}

	sendError(req.ID, InvalidParams, "Not implemented", nil)
}

// handleDocumentSymbol processes the 'textDocument/documentSymbol' request
func handleDocumentSymbol(server *Server, req RPCRequest) {
	var params DocumentSymbolParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		sendError(req.ID, InvalidParams, "Invalid params", nil)
		return
	}

	sendError(req.ID, InvalidParams, "Not implemented", nil)
}

func handleFormatting(server *Server, req RPCRequest) {
	var params DocumentFormattingParams
	err := json.Unmarshal(req.Params, &params)
	if err != nil {
		sendError(req.ID, InvalidParams, "Invalid params", nil)
		return
	}

	path := uriToPath(params.TextDocument.URI)
	content, err := server.cache.GetOrLoadFileContent(path)
	if err != nil {
		sendError(req.ID, InternalError, "Error reading file", nil)
		return
	}

	cmd := exec.Command(server.boostrapPath, "-format", "-")
	cmd.Stdin = strings.NewReader(strings.Join(content, "\n"))

	stdout, err := cmd.Output()
	if err != nil {
		sendError(req.ID, InternalError, "Error formatting file", nil)
		return
	}

	var edits []TextEdit

	edits = append(edits, TextEdit{
		Range: Range{
			Start: Position{Line: 0, Character: 0},
			End:   Position{Line: len(content), Character: 0},
		},
		NewText: string(stdout),
	})

	sendResult(req.ID, edits)
}

// readFileLines reads the content of a file and returns it as a slice of lines
func readFileLines(filePath string) ([]string, error) {
	contentBytes, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}
	content := string(contentBytes)
	lines := strings.Split(content, "\n")
	return lines, nil
}

var semaRegex = regexp.MustCompile(`([^:]*):(\d+):(\d+): (.+)`)

func semaFile(s *Server, path string) {
	content, err := s.cache.GetOrLoadFileContent(path)
	if err != nil {
		return
	}

	relPath, err := filepath.Rel(s.rootPath, path)
	if err != nil {
		log.Printf("Error making rel path: %v", err)
		return
	}

	// brio with the input from the file.
	cmd := exec.Command(s.boostrapPath, "-sema-lsp", "-stdin-filename", relPath)
	cmd.Dir = s.rootPath
	cmd.Stdin = strings.NewReader(strings.Join(content, "\n"))
	cmd.Stdout = nil

	stderr, err := cmd.StderrPipe()
	if err != nil {
		log.Printf("failed to get stderr from brio command: %v", err)
		return
	}

	if err := cmd.Start(); err != nil {
		log.Printf("failed to start brio command: %v", err)
		return
	}

	pid := cmd.Process.Pid
	scanner := bufio.NewScanner(stderr)

	entries := make(map[string][]Diagnostic)

	symbolDefs := make(map[string]*Symbol)
	symbolRefs := make(map[string][]SymbolRef)

	for scanner.Scan() {
		lineStr := scanner.Text()
		matches := semaRegex.FindStringSubmatch(lineStr)
		if len(matches) == 0 {
			continue
		}

		file := matches[1]
		line, _ := strconv.Atoi(matches[2])
		char, _ := strconv.Atoi(matches[3])
		msg := matches[4]

		// Line 0 with char 0 (and msg OK!) indicates no issues
		if line == 0 && char == 0 {
			entries[file] = []Diagnostic{}
			continue
		}

		pos := Position{Line: line - 1, Character: char - 1}
		if strings.HasPrefix(msg, "decl:") {
			parts := strings.Split(msg, ": ")
			if len(parts) != 3 {

				log.Printf("decl too many parts")
				continue
			}

			symbol := &Symbol{
				id:       parts[1] + strconv.Itoa(pid),
				name:     parts[2],
				position: pos,
				file:     file,
			}
			symbolDefs[symbol.id] = symbol
			symbolRefs[file] = append(symbolRefs[file], SymbolRef{
				id:       symbol.id,
				position: symbol.position,
				length:   len(symbol.name),
			})
			continue
		}

		if strings.HasPrefix(msg, "ref:") {
			parts := strings.Split(msg, ": ")
			if len(parts) != 3 {
				log.Printf("Ref too many parts")
				continue
			}

			id := parts[1] + strconv.Itoa(pid)
			length, _ := strconv.Atoi(parts[2])
			symbolRefs[file] = append(symbolRefs[file], SymbolRef{
				id:       id,
				position: pos,
				length:   length,
			})
			continue
		}

		entries[file] = append(entries[file], Diagnostic{
			Range:   Range{Start: pos, End: pos},
			Message: msg,
		})
	}

	if err := scanner.Err(); err != nil {
		log.Printf("error reading ctags output: %v", err)
		return
	}

	cmdErr := cmd.Wait()
	if cmdErr != nil {
		log.Printf("brio command failed: %v", cmdErr)
	}

	for path, diagnostics := range entries {
		diagnosticParams := PublishDiagnosticsParams{
			URI:         filepathToURI(path),
			Diagnostics: diagnostics,
		}
		sendNotification("textDocument/publishDiagnostics", diagnosticParams)
	}

	s.symbols.mu.Lock()
	defer s.symbols.mu.Unlock()

	// Add all new ids to the big id -> symbol map.
	maps.Copy(s.symbols.symbolByID, symbolDefs)

	// If sema was successful store the symbols for this file.
	if cmdErr == nil {
		s.symbols.symbolByRootPath[relPath] = slices.Collect(maps.Values(symbolDefs))
	}

	// For each successful (imported) file, store the refs.
	for path, diagnostics := range entries {
		if len(diagnostics) == 0 {
			s.symbols.symbolRefsByPath[path] = symbolRefs[path]
		}
	}
}

// findSymbolRangeInFile searches for the symbol in the specified line and returns its range
func findSymbolRangeInFile(lines []string, symbolName string, lineNumber int) Range {
	// Adjust line number to zero-based index
	lineIdx := lineNumber - 1
	if lineIdx < 0 || lineIdx >= len(lines) {
		// Line number out of range; return a zero range
		return Range{
			Start: Position{Line: lineIdx, Character: 0},
			End:   Position{Line: lineIdx, Character: 0},
		}
	}

	lineContent := lines[lineIdx]
	startChar := strings.Index(lineContent, symbolName)
	if startChar == -1 {
		// Symbol not found in the expected line; default to line start
		return Range{
			Start: Position{Line: lineIdx, Character: 0},
			End:   Position{Line: lineIdx, Character: len([]rune(lineContent))},
		}
	}

	// Calculate the end character position
	endChar := startChar + len([]rune(symbolName))

	return Range{
		Start: Position{Line: lineIdx, Character: startChar},
		End:   Position{Line: lineIdx, Character: endChar},
	}
}

// sendResult sends a successful JSON-RPC response
func sendResult(id json.RawMessage, result any) {
	response := RPCResponse{
		Jsonrpc: "2.0",
		ID:      id,
		Result:  result,
	}
	sendMessage(response)
}

// sendError sends an error JSON-RPC response
func sendError(id json.RawMessage, code int, message string, data any) {
	response := RPCResponse{
		Jsonrpc: "2.0",
		ID:      id,
		Error: &RPCError{
			Code:    code,
			Message: message,
			Data:    data,
		},
	}
	sendMessage(response)
}

func sendNotification(method string, params any) {
	notification := RPCNotification{
		Jsonrpc: "2.0",
		Method:  method,
		Params:  params,
	}
	sendMessage(notification)
}

// sendMessage marshals and sends the JSON-RPC response with appropriate headers
func sendMessage(resp any) {
	body, err := json.Marshal(resp)
	if err != nil {
		log.Printf("Error marshaling response: %v", err)
		return
	}

	// Write headers followed by the JSON body
	fmt.Printf("Content-Length: %d\r\n\r\n%s", len(body), string(body))
}

// uriToPath converts a file URI to a filesystem path
func uriToPath(uri string) string {
	if after, ok := strings.CutPrefix(uri, "file://"); ok {
		return filepath.FromSlash(after)
	}
	return uri
}

// filepathToURI converts a filesystem path to a file URI
func filepathToURI(path string) string {
	absPath, err := filepath.Abs(path)
	if err != nil {
		return ""
	}
	return "file://" + filepath.ToSlash(absPath)
}

// scanWorkspace runs ...
func (s *Server) scanWorkspace() error {
	files, err := listWorkspaceFiles(s.rootPath)
	if err != nil {
		return err
	}

	workers := runtime.NumCPU()
	size := (len(files) + workers - 1) / workers // calculate chunk size
	var wg sync.WaitGroup

	// start workers on file chunks
	for i := range workers {
		start := i * size
		if start >= len(files) {
			break
		}
		end := min(start+size, len(files))
		chunk := files[start:end]

		wg.Add(1)
		go func(chunk []string) {
			defer wg.Done()
		}(chunk)
	}

	wg.Wait() // wait for all workers
	return nil
}

// listWorkspaceFiles returns a list of relative file paths using git, jj, or a directory walk.
func listWorkspaceFiles(root string) ([]string, error) {
	// check git repo
	if isGitRepo(root) {
		out, err := exec.Command("git", "-C", root, "ls-files").Output()
		if err != nil {
			return nil, err
		}
		return strings.Split(strings.TrimSpace(string(out)), "\n"), nil
	}

	// check jujutsu repo
	if isJjRepo(root) {
		out, err := exec.Command("jj", "file", "list", "--repository", root).Output()
		if err != nil {
			return nil, err
		}
		return strings.Split(strings.TrimSpace(string(out)), "\n"), nil
	}

	// fallback: walk directory tree
	var files []string
	filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err == nil && !d.IsDir() {
			if rel, e := filepath.Rel(root, path); e == nil {
				files = append(files, rel)
			}
		}
		return nil
	})
	return files, nil
}

// isGitRepo checks if the directory is a Git repository
func isGitRepo(path string) bool {
	cmd := exec.Command("git", "-C", path, "rev-parse", "--is-inside-work-tree")
	return cmd.Run() == nil
}

// isJjRepo checks if the directory is a Jujutsu repository
func isJjRepo(path string) bool {
	cmd := exec.Command("jj", "repo", "info", "--repository", path)
	return cmd.Run() == nil
}

// getCurrentWord retrieves the current word at the given position in the document
func (s *Server) getCurrentWord(uri string, pos Position) (string, error) {
	filePath := uriToPath(uri)
	lines, err := s.cache.GetOrLoadFileContent(filePath)
	if err != nil {
		return "", fmt.Errorf("failed to load file content: %v", err)
	}

	if pos.Line >= len(lines) {
		return "", fmt.Errorf("line %d out of range", pos.Line)
	}

	line := lines[pos.Line]
	runes := []rune(line)
	if pos.Character > len(runes) {
		return "", fmt.Errorf("character %d out of range", pos.Character)
	}

	// Find word boundaries
	start := pos.Character
	for start > 0 && isIdentifierChar(runes[start-1]) {
		start--
	}

	end := pos.Character
	for end < len(runes) && isIdentifierChar(runes[end]) {
		end++
	}

	if start == end {
		return "", fmt.Errorf("no word found at position")
	}

	word := string(runes[start:end])
	return word, nil
}

// isIdentifierChar checks if a rune is a valid identifier character
func isIdentifierChar(c rune) bool {
	return (c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		(c >= '0' && c <= '9') ||
		c == '_' || c == '$'
}
