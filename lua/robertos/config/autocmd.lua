-- Set cwd directory on lsp attach
vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'LSP actions',
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client ~= nil then
      vim.api.nvim_set_current_dir(client.config.root_dir)
    end

    local s = vim.keymap.set
    s('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', { buffer = true, desc = 'LSP Hover Information' })
    s('n', 'gd', '<cmd>lua vim.lsp.buf.definition<cr>', { buffer = true, desc = 'LSP Jump To Definition' })
    s('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', { buffer = true, desc = 'LSP Jump To Declaration' })
    s('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', { buffer = true, desc = 'LSP List All Implementations' })
    s(
      'n',
      'go',
      '<cmd>lua vim.lsp.buf.type_definition()<cr>',
      { buffer = true, desc = 'LSP Jump To Definition of Type' }
    )
    s('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>', { buffer = true, desc = 'LSP List All References' })
    s('n', 'gs', '<cmd>lua vim.lsp.buf.signature_help()<cr>', { buffer = true, desc = 'LSP Display Signature Help' })
    s('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>', { buffer = true, desc = 'LSP Rename Symbol Under Cursor' })
    s(
      'n',
      '<F4>',
      '<cmd>lua vim.lsp.buf.code_action()<cr>',
      { buffer = true, desc = 'LSP Select Code Action Under Cursor' }
    )
    s(
      'n',
      'gl',
      '<cmd>lua vim.diagnostic.open_float()<cr>',
      { buffer = true, desc = 'LSP Show Diagnosic in Floating Window' }
    )
    s('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>', { buffer = true, desc = 'LSP Previous Diagnotic' })
    s('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>', { buffer = true, desc = 'LSP Next Diagnostic' })
  end,
})
