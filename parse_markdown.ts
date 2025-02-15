import { marked, type Token, type TokensList } from 'marked'

const lexer = new marked.Lexer({})
const tokens = lexer.lex(`
# Title
## Subtitle
This is a sentence \`with\` some code.
\`\`\`
f : A -> B
\`\`\`
`)

function trace(t: Token, depth = 0) {
  console.log(`${"    ".repeat(depth + 1)}${t.type}`)
  switch (t.type) {
    case 'blockquote': {
      t.tokens?.forEach(t => trace(t, depth + 1))
      break
    }
    case 'br': break
    case 'code': break
    case 'codespan': break
    case 'def': break
    case 'del': {
      t.tokens?.forEach(t => trace(t, depth + 1))
      break
    }
    case 'em': {
      t.tokens?.forEach(t => trace(t, depth + 1))
      break
    }
    case 'escape': break
    case 'heading': {
      t.tokens?.forEach(t => trace(t, depth + 1))
      break
    }
    case 'hr': break
    case 'html': break
    case 'image': break
    case 'link': {
      t.tokens?.forEach(t => trace(t, depth + 1))
      break
    }
    case 'list': {
      t.items.forEach((i: any) => trace(i, depth + i))
      break
    }
    case 'list_item': {
      t.tokens?.forEach(t => trace(t, depth + 1))
      break
    }
    case 'paragraph': {
      t.tokens?.forEach(t => trace(t, depth + 1))
      break
    }
    case 'space': break
    case 'strong': {
      t.tokens?.forEach(t => trace(t, depth + 1))
      break
    }
    // case 'table': break
    case 'tag': break
    case 'text': break
    default: throw new Error(`unhandled token type: ${t.type}`)
  }
}

tokens.forEach(t => trace(t))

type Tree
  = { type: "con", val: string, opts?: Record<string, string>, prms?: Record<string, string>, kids?: Tree[], }
  | { type: "list", kids: Tree[], }
  | string


function parse(t: Token, depth = 0): Tree[] {
  console.log(`${"    ".repeat(depth + 1)}${t.type}`)
  switch (t.type) {
    case 'blockquote': return [{ type: "con", val: "quoteBlock", kids: [{ type: "list", kids: t.tokens!.flatMap(t => parse(t, depth + 1)) }] }]
    case 'br': return []
    case 'code': return [{ type: "con", val: "codeBlock", prms: { value: `"${t.text}"` } }]
    case 'codespan': return [{ type: "con", val: "string", opts: { style: `inj'U @"code"` }, prms: { value: `"${t.text}"` } }]
    // case 'def': ...
    // case 'del': ...
    case 'em': return [{ type: "con", val: "string", opts: { style: `inj'U @"emphasis"` }, prms: { value: `"${t.text}"` } }]
    // case 'escape': ...
    // TODO: have to fix nesting
    case 'heading': {
      if (t.depth == 0) {
        return [{ type: "con", val: "page", kids: [{ type: "list", kids: t.tokens!.flatMap(t => parse(t, depth + 1)) }] }]
      } else {
        return [{ type: "con", val: "section", kids: [{ type: "list", kids: t.tokens!.flatMap(t => parse(t, depth + 1)) }] }]
      }
    }
    // case 'hr': ...
    // case 'html': ...
    case 'image': return [{ type: "con", val: "image", prms: { url: t.href }, kids: ["Nothing"] }]
    case 'link': return [{ type: "con", val: "linkExternal", opts: { url: `Just "${t.href}"` }, kids: [{ type: "con", val: "sentence", kids: [{ type: "list", kids: t.tokens!.flatMap(t => parse(t, depth + 1)) }] }] }]
    // case 'list': ...
    // case 'list_item': ...
    case 'paragraph': return [{ type: "con", val: "paragraph", kids: [{ type: "list", kids: t.tokens!.flatMap(t => parse(t, depth + 1)) }] }]
    case 'space': return []
    case 'strong': return [{ type: "con", val: "string", opts: { style: `inj'U @"emphasis"` }, prms: { value: `"${t.text}"` } }]
    // case 'table': ...
    // case 'tag': ...
    case 'text': return [{ type: "con", val: "string", prms: { value: `"${t.text}"` } }]
    default: throw new Error(`unhandled token type: ${t.type}`)
  }
}

function showRecord(r: Record<string, string> | undefined): string {
  return r === undefined ? "{}" : `{ ${Object.entries(r!).map(([k, v]) => `${k}: ${v}`).join(", ")} }`
}

function showTree(t: Tree): string {
  if (typeof t === "string") return t
  switch (t.type) {
    case 'con': {
      if (t.kids == undefined) return `(${t.val} ${showRecord(t.opts)} ${showRecord(t.prms)})`
      return `(${t.val} ${t.opts === undefined ? "{}" : JSON.stringify(t.opts)} ${t.prms === undefined ? "{}" : JSON.stringify(t.prms)} ${t.kids!.map(t => showTree(t)).join(" ")}`
    }
    case 'list': return `[${t.kids.map(t => showTree(t)).join(", ")}]`
  }
}

tokens.forEach(t => console.log(parse(t)))
tokens.forEach(t => parse(t).forEach(t => console.log(showTree(t))))
