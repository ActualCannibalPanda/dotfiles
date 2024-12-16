return {
  setup = function(capabilities)
    require('lspconfig').ols.setup({
      capabilities = capabilities,
    })
    vim.keymap.set(
      'n',
      '<localleader>rr',
      '<cmd>:TermExec cmd="odin run . -debug" direction=float<cr>',
      { desc = 'Run odin project' }
    )
  end,
}