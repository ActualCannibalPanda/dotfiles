return {
  'nvim-treesitter/nvim-treesitter',
  lazy = false,
  branch = 'main',
  dependencies = { 'HiPhish/rainbow-delimiters.nvim' },
  build = ':TSUpdate',
  config = function()
    local ts = require('nvim-treesitter')
    ts.setup()
    local fts = {
      'lua',
      'rust',
      'c',
      'cpp',
      'toml',
      'zig',
      'odin',
      'gdscript',
    }

    ts.install(fts)

    for _, ft in pairs(fts) do
      vim.api.nvim_create_autocmd('FileType', {
        pattern = { ft },
        callback = function()
          vim.treesitter.start()
          vim.wo[0][0].foldexpr = 'v:lua.vim.treesitter.foldexpr()'
          vim.wo[0][0].foldmethod = 'expr'
          vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
        end,
      })
    end

    require('rainbow-delimiters.setup').setup({})
  end,
}
