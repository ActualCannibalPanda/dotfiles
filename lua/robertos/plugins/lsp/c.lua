return {
	setup = function(capabilities)
		require("lspconfig").clangd.setup({ capabilities = capabilities })
	end,
}
