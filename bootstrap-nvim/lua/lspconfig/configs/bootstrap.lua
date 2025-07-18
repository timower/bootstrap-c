return {
	default_config = {
		cmd = { "bootsrap-lsp" },
		filetypes = { "bootstrap" },
		root_dir = function()
			return vim.fn.getcwd()
		end,
	},
	docs = {
		description = [[
Bootstrap Language Server
		]],
	},
}
