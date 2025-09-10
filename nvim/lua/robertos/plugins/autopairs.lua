return {
  'windwp/nvim-autopairs',
  event = 'InsertEnter',
  config = function(_, opts)
    require('nvim-autopairs').setup({
      check_ts = true,
      ts_config = {
        lua = {
          'string',
        },
      },
    })
  end,
}
