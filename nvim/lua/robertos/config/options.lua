function init()
  local o = vim.opt

  o.whichwrap = 'hl'

  -- line numbers
  o.signcolumn = 'auto:2'
  o.foldcolumn = 'auto:1'
  o.number = true
  o.relativenumber = true

  -- mouse
  o.mouse = 'a'
  o.mousescroll = 'ver:8,hor:6'
  o.mousemoveevent = true

  if vim.g.neovide then
    o.guifont = 'FiraCode Nerd Font'
  end

  vim.wo.foldmethod = 'expr'
  vim.wo.foldexpr = 'v:lua.vim.tressitter.foldexpr()'
  vim.o.foldlevelstart = 99
end

return {
  init = init,
}
