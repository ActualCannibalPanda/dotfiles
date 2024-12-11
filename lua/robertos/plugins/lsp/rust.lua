return {
  setup = function(capabilities)
    require('lspconfig').rust_analyzer.setup({ capabilities = capabilities })
  end,
}
