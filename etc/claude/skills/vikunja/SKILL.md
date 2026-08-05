---
name: vikunja
description: Austin's personal task board (Vikunja at todo.anardil.net) — read what's queued for a repo, file discovered work back onto it, move cards. TRIGGER when asked what's on the todo list / board / kanban, when asked to add or close a task, when starting substantial work in a repo that has board items, or when work surfaces a follow-up worth keeping past this session.
---

# Vikunja board

Austin's task board, self-hosted in the `todo` jail on vega. One shared project,
`Computers`, holding work across every repo in `~/src`; repos are distinguished by
**label**, not by project.

```sh
~/src/infrastructure/todo/opt/vikunja.sh <command>
```

That path is the only copy — don't vendor a second one. It reads its token from
`todo/claude.token` beside it. **LAN only**: this works from Austin's machine and
fails from any cloud-side agent.

| | |
|---|---|
| `board` | kanban buckets and their tasks |
| `tasks` | open items, list view |
| `labels` | labels available to tag with |
| `show <id>` | one task in full |
| `capture <label> <title...>` | file a task into `Someday` for triage |
| `start <id>` | move to Doing |
| `done <id>` / `reopen <id>` | close / reopen |
| `comment <id> <text...>` | add a comment |
| `move <id> <bucket>` | any bucket; names match on prefix |
| `api <METHOD> <path> [body]` | raw escape hatch |

Buckets: `Someday` → `To-Do` → `Doing` → `Done`.

## How to use it

**Capture is the point.** When work surfaces something real but out of scope — a
missing eviction path, a config that will break at the next rollover, a follow-up
you're deliberately not doing now — file it instead of leaving it in scrollback:

```sh
vikunja.sh capture diving "thumbnail cache has no eviction path"
```

Don't ask first. Filing is cheap and reversible, and `Someday` is a triage bucket,
not a commitment. Do mention it in your reply so it isn't a silent write.

**Read the board when it's relevant, not reflexively.** Starting substantial work
in a repo that has a label is a good moment (`vikunja.sh tasks`, look for that
label). A one-line fix is not.

**Bucket moves:**

- `start` (→ Doing) is free — it reports what's actually happening.
- `done` needs Austin's confirmation first. Ask, then close.
- Promoting `Someday` → `To-Do` is **his** call, never yours. That boundary is
  what keeps the board his intent rather than your judgment.

**Labels are existing-only.** `capture` fails on an unknown label by design: a
label created by the `claude` user would be owned by it and wouldn't appear in
Austin's own label list. If a repo needs a new label, ask him to make it in the
web UI.

**Not a replacement for your in-session todo list.** That's ephemeral scratch for
the task in hand. This board is the durable layer — things that outlive the
session. Never mirror one into the other.

## Traps

The wrapper absorbs these; they matter if you reach for `api` directly.

- **Reading the board is not `/buckets`.** `GET …/views/{kanban}/buckets` returns
  metadata only — every `count` is `0`, no tasks embedded, which reads exactly
  like an empty board. The real read is `…/views/{kanban}/tasks`.
- **`POST /tasks/{id}` is a whole-object update.** A bare `{"done":true}` blanks
  every field you omit. Read-modify-write.
- **`bucket_id` from `GET /tasks/{id}` is always `0`** — it's view-scoped.
- **A missing token scope 401s as "invalid token"**, identical to a garbage
  token. If some routes work and one doesn't, it's the scope. Don't conclude the
  token is broken and don't ask for a new one — name the route that failed.

Full API spec: `https://todo.anardil.net/api/v1/docs.json`. Jail, token, and user
setup: `~/src/infrastructure/todo/README.md`.
