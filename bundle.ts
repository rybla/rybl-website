import { serve_dir } from "./output/Rybl.Constants";

const proc = Bun.spawn([
  "bun",
  "spago", "bundle",
  "--module", "Rybl.App",
  "--bundle-type", "app",
  "--outfile", `${serve_dir}/main.js`
], {
  stdout: "inherit",
  stderr: "inherit",
});
await proc.exited;
