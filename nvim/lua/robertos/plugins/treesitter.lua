return {
  'nvim-treesitter/nvim-treesitter',
  branch = 'main',
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
        'gdscript',
      },
      auto_install = true,
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },
      indent = { enable = true },
    })

    if vim.loop.os_uname().sysname == 'Windows_NT' then
      require('nvim-treesitter.install').compilers = { 'zig' }
    end
    require('rainbow-delimiters.setup').setup({})
  end,
}
