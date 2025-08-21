local on_attach = require('robertos.plugins.lsp.on_attach')
return {
  setup = function(capabilities)
    require('lspconfig').lua_ls.setup({
      capabilities = capabilities,
      on_attach = on_attach,
      on_init = function(client)
        if client.workspace_folders then
          local path = client.workspace_folders[1].name
          if vim.uv.fs_stat(path .. '/.luarc.json') or vim.uv.fs_stat(path .. '/.luarc.jsonc') then
            return
          end
        end
      end,
      settings = {
        Lua = {},
      },
    })
  end,
}
