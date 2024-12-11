-- Set cwd directory on lsp attach
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)
		if client ~= nil then
			vim.api.nvim_set_current_dir(client.config.root_dir)
		end
	end,
})
