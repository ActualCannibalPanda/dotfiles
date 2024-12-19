return function(client, bufnr)
  if client ~= nil then
    vim.api.nvim_set_current_dir(client.config.root_dir)
  end

  local function set_buffer_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end

  set_buffer_keymap(
    'n',
    'K',
    '<cmd>lua vim.lsp.buf.hover()<cr>',
    { noremap = true, silent = true, desc = 'LSP Hover Information' }
  )
  set_buffer_keymap(
    'n',
    'gd',
    '<cmd>lua vim.lsp.buf.definition<cr>',
    { noremap = true, silent = true, desc = 'LSP Jump To Definition' }
  )
  set_buffer_keymap(
    'n',
    'gD',
    '<cmd>lua vim.lsp.buf.declaration()<cr>',
    { noremap = true, silent = true, desc = 'LSP Jump To Declaration' }
  )
  set_buffer_keymap(
    'n',
    'gi',
    '<cmd>lua vim.lsp.buf.implementation()<cr>',
    { noremap = true, silent = true, desc = 'LSP List All Implementations' }
  )
  set_buffer_keymap(
    'n',
    'go',
    '<cmd>lua vim.lsp.buf.type_definition()<cr>',
    { noremap = true, silent = true, desc = 'LSP Jump To Definition of Type' }
  )
  set_buffer_keymap(
    'n',
    'gr',
    '<cmd>lua vim.lsp.buf.referenceset_buffer_keymap()<cr>',
    { noremap = true, silent = true, desc = 'LSP List All References' }
  )
  set_buffer_keymap(
    'n',
    'gs',
    '<cmd>lua vim.lsp.buf.signature_help()<cr>',
    { noremap = true, silent = true, desc = 'LSP Display Signature Help' }
  )
  set_buffer_keymap(
    'n',
    '<F2>',
    '<cmd>lua vim.lsp.buf.rename()<cr>',
    { noremap = true, silent = true, desc = 'LSP Rename Symbol Under Cursor' }
  )

  set_buffer_keymap(
    'n',
    '<leader>ca',
    "<cmd>lua require('tiny-code-action').code_action()<cr>",
    { noremap = true, silent = true, desc = 'Range code action.' }
  )
  set_buffer_keymap(
    'n',
    'gl',
    '<cmd>lua vim.diagnostic.open_float()<cr>',
    { noremap = true, silent = true, desc = 'LSP Show Diagnosic in Floating Window' }
  )
  set_buffer_keymap(
    'n',
    '[d',
    '<cmd>lua vim.diagnostic.goto_prev()<cr>',
    { noremap = true, silent = true, desc = 'LSP Previous Diagnotic' }
  )
  set_buffer_keymap(
    'n',
    ']d',
    '<cmd>lua vim.diagnostic.goto_next()<cr>',
    { noremap = true, silent = true, desc = 'LSP Next Diagnostic' }
  )
end
