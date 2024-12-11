return {
	"neovim/nvim-lspconfig",
	config = function()
		local capabilities = require("cmp_nvim_lsp").default_capabilities()
		require("robertos.plugins.lsp.rust").setup(capabilities)
		require("robertos.plugins.lsp.lua").setup(capabilities)
		require("robertos.plugins.lsp.python").setup(capabilities)
	end,
}
