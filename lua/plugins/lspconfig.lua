return {
	"neovim/nvim-lspconfig",
	config = function()
		local capabilities = require("cmp_nvim_lsp").default_capabilities()
		require("plugins.lsp.rust").setup(capabilities)
		require("plugins.lsp.lua").setup(capabilities)
		require("plugins.lsp.python").setup(capabilities)
	end,
}
