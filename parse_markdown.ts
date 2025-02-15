import { marked, type Token, type TokensList } from 'marked'

const lexer = new marked.Lexer({})
// const tokens = lexer.lex(`
// # Page
// ## Subtitle 1
// ### Subtitle 1.1
// ## Subtitle 2
// `)
// const tokens = lexer.lex(`
// # Page
// Hello world
// `)
const tokens = lexer.lex(`
Two years ago, I described a view I was developing that science is grounded in
an implicit monotheism:
[On God: Science and Modernity](https://metagnosis.simplecast.com/episodes/on-god-science-modernity).
This essay is a short exposition of a more developed version of this view: A
Monotheistic Foundation for the Epistemology of Science.

# Two Axioms

The epistemology of science can be formulated as having two axioms:

- The axiom of an _universal objectivity_: the universe (everything that can be
  observed) is objective
- The axiom of _natural induction_: _natural_ patterns in some observations are
  some evidence that those patterns also (will) hold in other observations. (The
  predicate "natural" here is left as a definition that must be accounted for my
  particular accounts of natural induction.)

Note that the applicability of _Bayesian reasoning_ requires universal
objectivity since Bayesian reasoning relies on the fact that each observation is
made in the same universe -- even if the observations happen at different times,
places, and observers.

The axiom of natural induction is required for any reasoning beyond the purely
deductive (i.e. logic and, arguably, mathematics), and the _natural_ restriction
ensures that not all possible inductions are considered valid (i.e. they must be
_natural_ inductions). The question of which conditions qualify an induction as
natural is specified by a particular account of natural induction, which should
be resolved by an empirical investigation into how natural induction is actually
used (i.e. "natural induction" definitionally refers to whatever we are using
when we agree with some inductive arguments and disagree with others).

These two are in fact axioms because they are assumed in the epistemology of
science and are not derivable.

- Universal objectivity is not derivable since, without it, it is consistent to
  assume that universal subjectivity is false.
- The axiom of natural induction is not derivable since it makes no deductive
  claims.
  - Note that natural induction is commonly "derived" invalidly by appealing to
    itself (i.e. "natural induction is true because it’s made true predictions
    in the past"), but circular arguments are inconsistent.

# God

Universal objectivity implies that there is exactly one universe, as the
universe is everything that can be observed and all observers observe the same
universe.

In the universe, natural induction implies that there are patterns that have no
further explanation (as they appeal to only induction). (Claim #1) This arises
from the fact that there must be finitely many downward levels of explanation in
any explanation hierarchy. (Claim #2) Patterns in the _base_ (lowest level) of
an explanation hierarchy only appeal to induction. Note that the choice of
hierarchy here is irrelevant as long as the choice is consistent. Typically
scientists use a roughly standardized hierarchy of explanations, for example in
order from higher-to-lower level:

> ... > Biology > Chemistry > Physics

Biological patterns can be explained in terms of biological, chemical, or
physical patterns. Physical patterns can be explained in terms of physical
patterns but not in terms of biological patterns.

All this goes to show that the epistemology of science postulates a single
objective universe in which basic and unquestionable patterns (which we may be
unable to access the truth of) that ultimately account for everything that we
observe.

A simple way of describing this set of beliefs is that the epistemology of
science postulates some inaccessible source that accounts for why there are the
basic patterns in the universe that there are.

TODO: Make the end of this section more clear and better argued, right now its a
little incoherent

This inaccessible source is outside of the universe, as it is postulated as
unobservable (otherwise induction would not be an axiom). The scientific project
is to describe patterns at different levels of abstraction above the base level,
which ultimately is the standard by which the accuracy of all of these
descriptions is measured. In other words, everything that science can/attempts
to describe -- that is, everything is the universe -- is accounted for by appeal
to that inaccessible source. I call this source God.

This God may not have many of the aspects of the God of many religions but,
certainly in common with those ideas, this idea of God describes Him as
omnipotent and omniscient. I do not attempt to take a stance here on the
omnibenevolence of this God, but I think it can be argued.

If you don’t want to _call_ this source God, that’s fine. But I think "God" is a
fitting name for such an extra-universal entity that is the basis for
understanding reality.

# Proofs

Proof of claim #1. Cyclical hierarchies of explanations are inconsistent because
an explanation cannot be in terms of itself. Hierarchies of explanations that,
given a particular level, have an infinite depth are viscous regresses because
some explanations at this level can only be accounted for after infinitely many
steps. So, the only kind of hierarchy of explanations left is of finite
hierarchies.

Proof of claim #2. Induction is required to establish non-deductive facts and
there are no lower layers to appeal to in the explanation, and induction is an
axiom and so as an explanation for these patterns it is not derivable.
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

// tokens.forEach(t => trace(t))

type Tree
  = { type: 'con', con: TreeCon0, opts?: Record<string, string>, prms?: Record<string, string>, kids_mode: "0", kids?: [] }
  | { type: 'con', con: TreeCon1, opts?: Record<string, string>, prms?: Record<string, string>, kids_mode: "1", kids: [Tree] }
  | { type: 'con', con: TreeConMaybe1, opts?: Record<string, string>, prms?: Record<string, string>, kids_mode: "Maybe1", kids?: [Tree] }
  | { type: 'con', con: TreeCon2, opts?: Record<string, string>, prms?: Record<string, string>, kids_mode: "2", kids: [Tree, Tree] }
  | { type: 'con', con: TreeConArray, opts?: Record<string, string>, prms?: Record<string, string>, kids_mode: "Array", kids: Tree[], }
  | { type: 'con', con: TreeConSec, opts?: Record<string, string>, prms?: Record<string, string>, kids_mode: "Array", kids: Tree[], depth: number, }
  | { type: 'root', kids: Tree[] }
  | string

type TreeConSec = 'page' | 'section'
type TreeConArray = 'paragraph' | 'sentence'
type TreeCon2 = 'sidenote'
type TreeCon1 = 'linkExternal' | 'linkInternal' | 'quoteBlock' | 'error'
type TreeConMaybe1 = 'image'
type TreeCon0 = 'ref' | 'codeBlock' | 'mathBlock' | 'string'

function parse(t: Token, depth = 0): Tree[] {
  // console.log(`${"    ".repeat(depth + 1)}${t.type}`)
  switch (t.type) {
    case 'blockquote': return [{ type: 'con', con: "quoteBlock", kids: [{ type: 'con', con: 'paragraph', kids_mode: "Array", kids: t.tokens!.flatMap(t => parse(t, depth + 1)) }], kids_mode: "1" }]
    case 'br': return []
    case 'code': return [{ type: 'con', con: "codeBlock", prms: { value: `"${t.text}"` }, kids_mode: "0" }]
    case 'codespan': return [{ type: 'con', con: "string", opts: { style: `inj'U @"code"` }, prms: { value: `"${t.text}"` }, kids_mode: "0" }]
    // case 'def': ...
    // case 'del': ...
    case 'em': return [{ type: 'con', con: "string", opts: { style: `inj'U @"emphasis"` }, prms: { value: `"${t.text}"` }, kids_mode: "0" }]
    // case 'escape': ...
    // TODO: have to fix nesting
    case 'heading': {
      if (t.depth == 0) {
        return [{ type: 'con', con: "page", prms: { title: `"${t.text}"` }, depth: t.depth, kids: [], kids_mode: "Array" }]
      } else {
        return [{ type: 'con', con: "section", prms: { title: `"${t.text}"` }, depth: t.depth, kids: [], kids_mode: "Array" }]
      }
    }
    // case 'hr': ...
    // case 'html': ...
    case 'image': return [{ type: 'con', con: "image", prms: { url: t.href }, kids_mode: "Maybe1" }]
    case 'link': return [{ type: 'con', con: "linkExternal", opts: { url: `Just "${t.href}"` }, kids: [{ type: 'con', con: "sentence", kids: t.tokens!.flatMap(t => parse(t, depth + 1)), kids_mode: "Array" }], kids_mode: "1" }]
    // case 'list': ...
    // case 'list_item': ...
    case 'paragraph': return [{ type: 'con', con: "paragraph", kids: t.tokens!.flatMap(t => parse(t, depth + 1)), kids_mode: "Array" }]
    case 'space': return []
    case 'strong': return [{ type: 'con', con: "string", opts: { style: `inj'U @"emphasis"` }, prms: { value: `"${t.text}"` }, kids_mode: "0" }]
    // case 'table': ...
    // case 'tag': ...
    case 'text': return [{ type: 'con', con: "string", prms: { value: `"${t.text}"` }, kids_mode: "0" }]
    default: throw new Error(`unhandled token type: ${t.type}`)
  }
}

type TreeWithKids = Tree & { kids: Tree[] }
type End = TreeWithKids & { depth: number }

function assembleTrees(ts: Tree[]): Tree[] {
  var end: End = { type: 'root', kids: [], depth: -1 }
  var path: End[] = [end]
  ts.forEach(t => {
    console.log("•", JSON.stringify(path), JSON.stringify(end))
    if (typeof t === 'string') {
      end.kids.push(t)
    } else {
      switch (t.type) {
        case 'con': {
          switch (t.con) {
            case 'page': {
              path.push(end)
              end = t as End
              break
            }
            case 'section': {
              const d_depth = t.depth - end.depth - 1
              console.log("section", { "t.depth": t.depth, "end.depth": end.depth, "d_depth": d_depth })
              if (d_depth < 0) {
                for (var i = 0; i > d_depth; i--) end = path.pop()!
                end.kids.push(t)
                end = t as End
              } else {
                end.kids.push(t)
                path.push(end)
                end = t as End
              }
              break
            }
            default: {
              end.kids.push(t)
              break
            }
          }
          break
        }
        case 'root': break
      }
    }
  })
  return path[0].kids
}

function showRecord(r: Record<string, string> | undefined): string {
  return r === undefined ? "{}" : `{${Object.entries(r!).map(([k, v]) => `${k}: ${v}`).join(", ")}}`
}

function showTree(t: Tree): string {
  if (typeof t === "string") return t
  switch (t.type) {
    case 'con': {
      if (t.kids == undefined) { return `(${t.con} ${showRecord(t.opts)} ${showRecord(t.prms)})` }
      switch (t.kids_mode) {
        case '0': return `(${t.con} ${showRecord(t.opts)} ${showRecord(t.prms)})`
        case '1': return `(${t.con} ${showRecord(t.opts)} ${showRecord(t.prms)}) ${showTree(t.kids[0])}`
        case 'Maybe1': return `(${t.con} ${showRecord(t.opts)} ${showRecord(t.prms)}) ${t.kids === undefined ? "Nothing" : `Just ${showTree(t.kids[0])}`})`
        case '2': return `(${t.con} ${showRecord(t.opts)} ${showRecord(t.prms)}) ${showTree(t.kids[0])} ${showTree(t.kids[1])})`
        case 'Array': return `(${t.con} ${showRecord(t.opts)} ${showRecord(t.prms)} $ sequence $ [${t.kids!.map(t => showTree(t)).join(", ")}])`
      }
    }
    case 'root': return `ROOT[${t.kids.map(t => showTree(t)).join(", ")}]`
  }
}

// tokens.forEach(t => console.log(JSON.stringify(t, undefined, "   ")))
// tokens.forEach(t => console.log(JSON.stringify(parse(t), undefined, "   ")))
// tokens.forEach(t => parse(t).forEach(t => console.log(showTree(t))))

const trees = tokens.flatMap(t => parse(t))
// console.log(trees)
trees.forEach(tree => console.log(showTree(tree)))
const root_kids = assembleTrees(trees)
root_kids.forEach(t => console.log(showTree(t)))
