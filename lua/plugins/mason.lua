return {
	{
		"williamboman/mason.nvim",
		dependencies = { { "williamboman/mason-lspconfig.nvim", module = "mason" } },
		config = function()
			require("mason").setup({
				ui = {
					icons = {
						package_installed = "",
						package_pending = "",
						package_uninstalled = "",
					},
				},
			})
			require("mason-lspconfig").setup({
				ensure_installed = { "lua_ls", "rust_analyzer", "clangd" },
				automatic_install = true,
			})
		end,
	},
}
