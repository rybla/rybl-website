const proc = Bun.spawn([
  "bun",
  "spago", "run",
  "-m", "Rybl.Compile",
], {
  stdout: "inherit",
  stderr: "inherit",
});
await proc.exited;