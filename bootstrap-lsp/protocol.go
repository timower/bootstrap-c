package main

import "encoding/json"

const (
	// Defined by JSON-RPC
	ParseError     = -32700
	InvalidRequest = -32600
	MethodNotFound = -32601
	InvalidParams  = -32602
	InternalError  = -32603

	/**
	 * This is the start range of JSON-RPC reserved error codes.
	 * It doesn't denote a real error code. No LSP error codes should
	 * be defined between the start and end range. For backwards
	 * compatibility the `ServerNotInitialized` and the `UnknownErrorCode`
	 * are left in the range.
	 *
	 * @since 3.16.0
	 */
	jsonrpcReservedErrorRangeStart = -32099
	/** @deprecated use jsonrpcReservedErrorRangeStart */
	serverErrorStart = jsonrpcReservedErrorRangeStart

	/**
	 * Error code indicating that a server received a notification or
	 * request before the server received the `initialize` request.
	 */
	ServerNotInitialized = -32002
	UnknownErrorCode     = -32001

	/**
	 * This is the end range of JSON-RPC reserved error codes.
	 * It doesn't denote a real error code.
	 *
	 * @since 3.16.0
	 */
	jsonrpcReservedErrorRangeEnd = -32000
	/** @deprecated use jsonrpcReservedErrorRangeEnd */
	serverErrorEnd = jsonrpcReservedErrorRangeEnd

	/**
	 * This is the start range of LSP reserved error codes.
	 * It doesn't denote a real error code.
	 *
	 * @since 3.16.0
	 */
	lspReservedErrorRangeStart = -32899

	/**
	 * A request failed but it was syntactically correct, e.g the
	 * method name was known and the parameters were valid. The error
	 * message should contain human readable information about why
	 * the request failed.
	 *
	 * @since 3.17.0
	 */
	RequestFailed = -32803

	/**
	 * The server cancelled the request. This error code should
	 * only be used for requests that explicitly support being
	 * server cancellable.
	 *
	 * @since 3.17.0
	 */
	ServerCancelled = -32802

	/**
	 * The server detected that the content of a document got
	 * modified outside normal conditions. A server should
	 * NOT send this error code if it detects a content change
	 * in its unprocessed messages. The result even computed
	 * on an older state might still be useful for the client.
	 *
	 * If a client decides that a result is not of any use anymore
	 * the client should cancel the request.
	 */
	ContentModified = -32801

	/**
	 * The client has canceled a request and a server has detected
	 * the cancel.
	 */
	RequestCancelled = -32800

	/**
	 * This is the end range of LSP reserved error codes.
	 * It doesn't denote a real error code.
	 *
	 * @since 3.16.0
	 */
	lspReservedErrorRangeEnd = -32800
)

// RPCRequest represents a JSON-RPC request structure
type RPCRequest struct {
	Jsonrpc string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params,omitempty"`
}

// RPCResponse represents a JSON-RPC response structure
type RPCResponse struct {
	Jsonrpc string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id"`
	Result  any             `json:"result,omitempty"`
	Error   *RPCError       `json:"error,omitempty"`
}

type RPCNotification struct {
	Jsonrpc string `json:"jsonrpc"`
	Method  string `json:"method"`
	Params  any    `json:"params,omitempty"`
}

// RPCError represents a JSON-RPC error object
type RPCError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
	Data    any    `json:"data,omitempty"`
}

// InitializeParams represents parameters for the 'initialize' request
type InitializeParams struct {
	RootURI string `json:"rootUri"`
}

// InitializeResult represents the result of the 'initialize' request
type InitializeResult struct {
	Capabilities ServerCapabilities `json:"capabilities"`
	Info         ServerInfo         `json:"serverInfo"`
}

// ServerCapabilities defines the capabilities of the language server
type ServerCapabilities struct {
	TextDocumentSync           *TextDocumentSyncOptions `json:"textDocumentSync,omitempty"`
	CompletionProvider         *CompletionOptions       `json:"completionProvider,omitempty"`
	DefinitionProvider         bool                     `json:"definitionProvider,omitempty"`
	WorkspaceSymbolProvider    bool                     `json:"workspaceSymbolProvider,omitempty"`
	DocumentSymbolProvider     bool                     `json:"documentSymbolProvider,omitempty"`
	DocumentFormattingProvider bool                     `json:"documentFormattingProvider,omitempty"`
}

// ServerInfo defines the server name and version
type ServerInfo struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// TextDocumentSyncOptions defines options for text document synchronization
type TextDocumentSyncOptions struct {
	Change    int  `json:"change"`
	OpenClose bool `json:"openClose"`
	Save      bool `json:"save"`
}

// CompletionOptions defines options for the completion provider
type CompletionOptions struct {
	TriggerCharacters []string `json:"triggerCharacters,omitempty"`
}

// WorkspaceSymbolParams represents the parameters for the 'workspace/symbol' request
type WorkspaceSymbolParams struct {
	Query string `json:"query"`
}

// DocumentSymbolParams represents the parameters for the 'textDocument/documentSymbol' request
type DocumentSymbolParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
}

// SymbolInformation represents information about a symbol
type SymbolInformation struct {
	Name          string   `json:"name"`
	Kind          int      `json:"kind"`
	Location      Location `json:"location"`
	ContainerName string   `json:"containerName,omitempty"`
}

// DidOpenTextDocumentParams represents the 'textDocument/didOpen' notification
type DidOpenTextDocumentParams struct {
	TextDocument TextDocument `json:"textDocument"`
}

// TextDocument represents a text document in the editor
type TextDocument struct {
	URI        string `json:"uri"`
	LanguageID string `json:"languageId"`
	Version    int    `json:"version"`
	Text       string `json:"text"`
}

// TextDocumentPositionParams represents the parameters used in requests that require a text document and position.
type TextDocumentPositionParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
}

// DidChangeTextDocumentParams represents the 'textDocument/didChange' notification
type DidChangeTextDocumentParams struct {
	TextDocument   TextDocumentIdentifier           `json:"textDocument"`
	ContentChanges []TextDocumentContentChangeEvent `json:"contentChanges"`
}

// TextDocumentIdentifier identifies a text document
type TextDocumentIdentifier struct {
	URI string `json:"uri"`
}

// TextDocumentContentChangeEvent represents a change in the text document
type TextDocumentContentChangeEvent struct {
	Text string `json:"text"`
}

// DidCloseTextDocumentParams represents the 'textDocument/didClose' notification
type DidCloseTextDocumentParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
}

// DidSaveTextDocumentParams represents the 'textDocument/didSave' notification
type DidSaveTextDocumentParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Text         string                 `json:"text,omitempty"`
}

// CompletionParams represents the 'textDocument/completion' request
type CompletionParams struct {
	TextDocument PositionParams `json:"textDocument"`
	Position     Position       `json:"position"`
}

// Position represents a position in a text document
type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"`
}

// PositionParams holds the URI for position-based requests
type PositionParams struct {
	URI string `json:"uri"`
}

// CompletionItem represents a completion suggestion
type CompletionItem struct {
	Label         string         `json:"label"`
	Kind          int            `json:"kind,omitempty"`
	Detail        string         `json:"detail,omitempty"`
	Documentation *MarkupContent `json:"documentation,omitempty"`
}

// MarkupContent represents documentation content
type MarkupContent struct {
	Kind  string `json:"kind"`
	Value string `json:"value"`
}

// CompletionList represents a list of completion items
type CompletionList struct {
	IsIncomplete bool             `json:"isIncomplete"`
	Items        []CompletionItem `json:"items"`
}

// Location represents a location in a text document
type Location struct {
	URI   string `json:"uri"`
	Range Range  `json:"range"`
}

// Range represents a range in a text document
type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

type Diagnostic struct {
	Range   Range  `json:"range"`
	Message string `json:"message"`
}

type PublishDiagnosticsParams struct {
	URI         string       `json:"uri"`
	Diagnostics []Diagnostic `json:"diagnostics"`
}

type DocumentFormattingParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	// Ignore options
}

type TextEdit struct {
	Range   Range  `json:"range"`
	NewText string `json:"newText"`
}