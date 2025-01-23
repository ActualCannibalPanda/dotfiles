local on_attach = require('robertos.plugins.lsp.on_attach')
return {
  setup = function(capabilities)
    require('lspconfig').clangd.setup({
      root_dir = require('lspconfig').util.root_pattern('meson.build', 'CMakeLists.txt', 'Makefile'),
      capabilities = capabilities,
      on_attach = on_attach,
    })
  end,
}
