return {
  'nvim-treesitter/nvim-treesitter',
  dependencies = { 'HiPhish/rainbow-delimiters.nvim' },
  build = ':TSUpdate',
  config = function()
    require('nvim-treesitter.configs').setup({
      ensure_installed = {
        'lua',
        'rust',
        'c',
        'cpp',
        'toml',
        'zig',
        'odin',
      },
      auto_install = true,
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },
      indent = { enable = true },
    })
    require('rainbow-delimiters.setup').setup({})
  end,
}
