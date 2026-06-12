return {
	default_config = {
		cmd = { "bootsrap-lsp" },
		filetypes = { "brio" },
		root_dir = function()
			return vim.fn.getcwd()
		end,
	},
	docs = {
		description = [[
Brio Language Server
		]],
	},
}
