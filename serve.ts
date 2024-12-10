const port = 8000;
const served_dir = "docs"

Bun.serve({
  port,
  async fetch(req) {
    const url = new URL(req.url);
    const filePath = `${served_dir}${url.pathname}`;
    const file = Bun.file(filePath);
    if (!(await file.exists())) return new Response(`Not Found: ${req.url}`, { status: 404 });
    return new Response(file);
  }
});

console.log(`serving at http://localhost:${port}`)

