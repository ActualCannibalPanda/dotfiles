return {
  'hrsh7th/nvim-cmp',
  dependencies = {
    'neovim/nvim-lspconfig',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/nvim-cmp',
    'saadparwaiz1/cmp_luasnip',
    'p00f/clangd_extensions.nvim',
  },
  config = function()
    local cmp = require('cmp')
    cmp.setup({
      enabled = function()
        local disabled = false
        disabled = disabled or (vim.api.nvim_get_option_value('buftype', { buf = 0 }) == 'prompt')
        disabled = disabled or (vim.fn.reg_recording() ~= '')
        disabled = disabled or (vim.fn.reg_executing() ~= '')
        disabled = disabled or require('cmp.config.context').in_treesitter_capture('comment')
        disabled = disabled or require('cmp.config.context').in_treesitter_capture('string')
        return not disabled
      end,
      snippet = {
        expand = function(args)
          local indent_nodes = true
          if vim.api.nvim_get_option_value('filetype', { buf = 0 }) == 'dart' then
            indent_nodes = false
          end
          require('luasnip').lsp_expand(args.body, {
            indent = indent_nodes,
          })
        end,
      },
      window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
      },
      mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }),
        ['<Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          else
            local ls = require('luasnip')
            if ls.locally_jumpable(1) then
              ls.jump(1)
            else
              fallback()
            end
          end
        end, { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          else
            local ls = require('luasnip')
            if ls.locally_jumpable(-1) then
              ls.jump(-1)
            else
              fallback()
            end
          end
        end, { 'i', 's' }),
      }),
      sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
        { name = 'buffer' },
        { name = 'renpy' },
      }),
    })

    local cmp_autopairs = require('nvim-autopairs.completion.cmp')
    cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())

    local capabilities = require('cmp_nvim_lsp')
    -- Lua
    vim.lsp.config('lua_ls', {
      settings = {
        Lua = {
          diagnostics = {
            globals = { 'vim' },
          },
        },
      },
    })
    vim.lsp.config('clangd', {
      cmd = { 'clangd' },
      filetypes = { 'c', 'cpp' },
      root_markers = {
        '.clang-format',
      },
    })
    vim.lsp.enable({
      'lua_ls',
      'pyright',
      'clangd',
      'cmake',
      'glsl_analyzer',
      'rust-analyzer',
      'wgsl_analyzer',
    })

    vim.keymap.set('n', '<F2>', vim.lsp.buf.rename)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover)
    -- vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action)
  end,
}
