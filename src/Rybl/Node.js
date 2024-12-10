import * as fs from 'node:fs/promises';

export const copy_ = (source) => (target) =>
  ({ recursive, force, errorOnExist }) => () =>
    fs.cp(source, target, { recursive, force, errorOnExist })
