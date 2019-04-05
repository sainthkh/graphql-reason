include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: Sys.getcwd() ++ "/test/__snapshots__",
      projectDir: Sys.getcwd(),
    });
});