return {
  setup = function(capabilities)
    require('lspconfig').ols.setup({
      capabilities = capabilities,
    })
  end,
}
