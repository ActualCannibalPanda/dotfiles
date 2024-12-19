local on_attach = require('robertos.plugins.lsp.on_attach')
return {
  setup = function(capabilities)
    require('lspconfig').ols.setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })
    vim.keymap.set(
      'n',
      '<localleader>rr',
      '<cmd>:TermExec cmd="odin run . -debug" direction=float<cr>',
      { desc = 'Run odin project' }
    )
  end,
}
