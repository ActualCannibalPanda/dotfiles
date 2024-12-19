local on_attach = require('robertos.plugins.lsp.on_attach')
return {
  setup = function(server, capabilities)
    require('lspconfig').zls.setup({ capabilities = capabilities, on_attach = on_attach })
  end,
}
