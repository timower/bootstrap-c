local M = {}

M.options = nil

local defaults = {
	conform = false,
	lua_lint = false,
	treesitter = true,
	lspconfig = true,
}

local scriptPath = debug.getinfo(1).source:sub(2)
local scriptDir = vim.fn.fnamemodify(scriptPath, ":p:h")
-- Go from <dir>/brio-nvim/lua/brio.lua to <dir>/
M.brioDir = vim.fn.simplify(scriptDir .. "../../../")

local pattern = [[([^:]*):(%d+):(%d+): (.+)]]
local groups = { "file", "lnum", "col", "message" }

function M.setup(opts)
	vim.filetype.add({
		extension = {
			b = "brio",
		},
	})
	M.options = vim.tbl_deep_extend("force", {}, defaults, opts or {})

	if M.options.conform then
		local conform = require("conform")
		conform.formatters.bformat = {
			command = M.brioDir .. "brio",
			args = { "-format", "-" },
			env = {
				ASAN_OPTIONS = "detect_leaks=0",
			},
		}
		conform.formatters_by_ft.brio = { "bformat" }
	end

	if M.options.lua_lint then
		local lint = require("lint")
		lint.linters_by_ft.brio = { "brio" }
		lint.linters.brio = {
			name = "brio",
			cmd = M.brioDir .. "brio",
			args = {
				"-sema",
				"-stdin-filename",
				function()
					return vim.api.nvim_buf_get_name(0)
				end,
			},
			stdin = true,
			stream = "stderr",
			ignore_exitcode = true,
			env = {
				ASAN_OPTIONS = "detect_leaks=0",
			},
			parser = require("lint.parser").from_pattern(pattern, groups, nil, {
				["source"] = "brio",
				["severity"] = vim.diagnostic.severity.ERROR,
			}),
		}
	end

	if M.options.treesitter then
		---@class ParserInfo
		vim.api.nvim_create_autocmd("User", {
			pattern = "TSUpdate",
			callback = function()
				require("nvim-treesitter.parsers").brio = {
					install_info = {
						path = M.brioDir .. "/tree-sitter-brio/",
					},
				}
			end,
		})
		-- local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

		-- parser_config.brio = {
		-- 	install_info = {
		-- 		url = M.brioDir .. "/tree-sitter-brio/",
		-- 		files = { "src/parser.c" }, -- note that some parsers also require src/scanner.c or src/scanner.cc
		-- 	},
		-- }
	end

	if M.options.lspconfig then
		require("lspconfig").brio.setup({
			cmd = {
				M.brioDir .. "/brio-lsp/brio-lsp",
				"-brio-path",
				M.brioDir .. "brio",
			},
		})
	end
end

return M
