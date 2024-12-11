return {
  setup = function(capabilities)
    require('lspconfig').pyright.setup({ capabilities = capabilities })
  end,
}
