# time-tracker

A simple CLI time tracking application with persistent storage, writtern in Haskell.

## Usage

Load or initialize a project:

```
time-tracker <project-name>
```

Projects are persistent, and stored as <project-name>.yaml
within the `baseDir` directory as specified at `$XDG_HOME_DIR/time-tracker/config.yaml`.

Once a project has been loaded, its current state (Paused / Running) and total time
will be displayed.

To refresh the display, press Return. To pause or unpause, press Space.

Quitting is accomplished via Q or Ctrl-C, both of which will
pause the current project if it is running,
and save its state to disk before exiting.
