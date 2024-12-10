return {
	setup = function(server, capabilities)
		require("lspconfig").zls.setup({ capabilities = capabilities })
	end,
}
