local M = {}

M.options = nil

local defaults = {
	conform = true,
	lua_lint = true,
	treesitter = true,
}

local bootstrapDirs = { "/home/timo/Projects/bootstrap-c/", "/Users/timo/Projects/Bootstrap/" }
M.bootstrapDir = nil
for _, c in ipairs(bootstrapDirs) do
	if vim.fn.isdirectory(c) == 1 then
		M.bootstrapDir = c
		break
	end
end

local pattern = [[([^:]*):(%d+):(%d+): (.+)]]
local groups = { "file", "lnum", "col", "message" }

function M.setup(opts)
	M.options = vim.tbl_deep_extend("force", {}, defaults, opts or {})

	if M.options.conform then
		local conform = require("conform")
		conform.formatters.bformat = {
			command = M.bootstrapDir .. "format",
			env = {
				ASAN_OPTIONS = "detect_leaks=0",
			},
		}
		conform.formatters_by_ft.bootstrap = { "bformat" }
	end

	if M.options.lua_lint then
		local lint = require("lint")
		lint.linters_by_ft.bootstrap = { "bootstrap" }
		lint.linters.bootstrap = {
			name = "bootstrap",
			cmd = M.bootstrapDir .. "bootstrap",
			stdin = false,
			ignore_exitcode = true,
			env = {
				ASAN_OPTIONS = "detect_leaks=0",
			},
			parser = require("lint.parser").from_pattern(pattern, groups, nil, {
				["source"] = "bootstrap",
				["severity"] = vim.diagnostic.severity.ERROR,
			}),
		}
	end

	if M.options.treesitter then
		---@class ParserInfo
		local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

		parser_config.bootstrap = {
			install_info = {
				url = M.bootstrapDir .. "/tree-sitter-bootstrap/",
				files = { "src/parser.c" }, -- note that some parsers also require src/scanner.c or src/scanner.cc
			},
		}
	end
end

return M
