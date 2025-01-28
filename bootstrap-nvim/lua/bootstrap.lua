local M = {}

M.options = nil

local defaults = {
	conform = true,
	lua_lint = true,
}

local scriptPath = debug.getinfo(1).source:sub(2)
local scriptDir = vim.fn.fnamemodify(scriptPath, ":p:h")
-- Go from <dir>/bootstrap-nvim/lua/bootstrap.lua to <dir>/
M.bootstrapDir = vim.fn.simplify(scriptDir .. "../../../")

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
end

return M
