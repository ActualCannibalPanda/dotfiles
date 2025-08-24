return {
  'nvim-orgmode/orgmode',
  event = 'VeryLazy',
  config = function()
    require('orgmode').setup({
      org_agenda_files = '~/org/**/*',
      org_default_notes_file = '~/org/notes.org',
      org_capture_templates = {
        h = {
          description = "Hack 'n' Slash Todo",
          template = '* TODO %?\n',
          target = '~/org/hacknslash.org',
          heading = 'Todos',
        },
        s = {
          description = 'Shooter Todo',
          template = '* TODO %?\n',
          target = '~/org/shooter.org',
          heading = 'Todos',
        },
      },
      org_log_done = false,
      org_agenda_skip_scheduled_if_done = false,
    })
  end,
}
