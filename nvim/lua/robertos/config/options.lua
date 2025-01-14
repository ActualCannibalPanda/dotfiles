function init()
  local o = vim.opt

  -- line numbers
  o.signcolumn = 'auto:2'
  o.foldcolumn = 'auto:1'
  o.number = true
  o.relativenumber = true

  -- mouse
  o.mouse = 'a'
  o.mousescroll = 'ver:8,hor:6'
  o.mousemoveevent = true

  -- tabs
  o.tabstop = 2
  o.vartabstop = '2'
  o.shiftwidth = 0
  o.softtabstop = -1
  o.expandtab = true

  if vim.g.neovide then
    o.guifont = 'FiraCode Nerd Font'
  end
end

return {
  init = init,
}
