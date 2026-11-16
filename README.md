# oeil

Follow a merged pull request until it is deployed, from a terminal or an
MCP client, by reading the web UI of an [OCurrent](https://github.com/ocurrent/ocurrent)
service such as [deploy.ci.ocaml.org](https://deploy.ci.ocaml.org).

OCurrent web UIs show their pipeline only as an SVG diagram for a browser.
oeil reads that diagram back into a graph, finds a branch's deployment chain
(build → push-manifest → pull → docker-service → notify), and follows one
commit through it. It ties each stage to the commit through the image
digests in the job logs. It needs no credentials for the OCurrent service.

For example, following the latest deploy of ocaml.org's `staging` branch
on 2026-11-16, whose last run had failed (output on a pipe, abridged):

```
$ oeil watch ocaml/ocaml.org:staging --no-logs
ocaml/ocaml.org staging (latest)
✓ picked up by the deployer  0.8s
✓ build linux-x86_64  e5b458339 → sha256:82fcd59ab930…  0.0s
✓ push ocurrent/v3.ocaml.org-server:staging  → sha256:0463d1dc4d3a…  0.0s
  ✗ pull for infra_www: Command "docker" "--context" "v3c.ocaml.org" "pull" …
✗ pull for infra_www  failed  2026-11-02/180130-docker-pull-c468ac  0.0s  failed
✗ deployment failed at pull:infra_www
```

followed by a summary table of every stage, its status and its job.

On a terminal, `oeil watch` is interactive: a live table of the stages,
redrawn at the bottom of the terminal
([console](https://github.com/samoht/ocaml-console)), where you can open the
log of any stage:

```
ocaml/ocaml.org main @ 823ea15fa   ✔ deployed
╭───┬───┬─────────────────────┬────────┬────────┬──────────╮
│   │   │ stage               │ status │ job    │     time │
├───┼───┼─────────────────────┼────────┼────────┼──────────┤
│ › │ ✔ │ build linux-x86_64  │ done   │ d516d2 │ 10:23:52 │
│   │ ✔ │ push ocurrent/      │ done   │ b8237c │ 10:44:07 │
│   │   │ v3.ocaml.org-server │        │        │          │
│   │   │ :live               │        │        │          │
│   │ ✔ │ pull for infra_www2 │ done   │ 4f0aba │ 10:44:30 │
│   │ ✔ │ pull for infra_www1 │ done   │ 4f0aba │ 10:44:30 │
│   │ ✔ │ service infra_www2  │ done   │ e3b420 │ 10:45:13 │
│   │ ✔ │ service infra_www1  │ done   │ 037a51 │ 10:45:13 │
│   │ ✔ │ notify              │ done   │ 53af31 │ 10:45:24 │
╰───┴───┴─────────────────────┴────────┴────────┴──────────╯
↑↓ select  ⏎ log  q quit
```

| Where | Keys |
|---|---|
| summary | `↑`/`↓` or `k`/`j` select a stage, `⏎` or `→` open its log, `q` or `Esc` quit |
| log | `↑`/`↓`, `PgUp`/`PgDn` scroll (pauses following), `g` top, `G` follow the end again, `Esc`, `←` or `q` back to the summary |

A running job's log is followed live. When the deployment ends, the verdict
shows in the header and the view stays open until you quit; the summary is
left in the scrollback.

With `--rows`, on a pipe, or with `--progress plain|quiet|json|tty`, it
prints scrolling rows instead: a running job's latest log lines show under
its row and fold away when it succeeds, and `--progress json` prints JSON
events.

## Following a repository

`oeil repo owner/repo` summarises what is heading to production, per
deployed branch:

```
$ oeil repo ocaml/ocaml.org --once
ocaml/ocaml.org   refreshed now
main   live 058758ce6 since 11:27:14   ● deploying 1ee8711f6: build running
╭───┬───┬───────┬─────────────────────────────────────┬───────────────┬────────────────╮
│   │   │ PR    │ title                               │ state         │           when │
├───┼───┼───────┼─────────────────────────────────────┼───────────────┼────────────────┤
│   │ ● │ #4020 │ attribution: 1 page (first_class_m… │ build running │ merged 17m ago │
│   │ ✔ │ #4005 │ Drop odoc from dev environments, d… │ live          │       11:27:14 │
│   │ ✔ │ #4019 │ data-packer: give each executable … │ live          │       10:45:13 │
│   │ ✔ │ #3822 │ deps: upgrade to OCaml 5.5.2        │ live          │       10:45:13 │
╰───┴───┴───────┴─────────────────────────────────────┴───────────────┴────────────────╯
staging   no successful deploy found recently   ✗ latest build e5b458339 failed at pull:infra_www
```

(2026-11-16, abridged.) An *active* PR is a merged PR whose commit is not
live yet, whatever its age or merge date, or one that went live in the last
`--recent` hours (default 24). Its state comes from where its merge commit
sits on the branch's first-parent history:

```
… live commit │ in the build being deployed │ merged since │ head
  live          deploying / failed            waiting
```

- The live commit comes from the deploy history, which is read back from
  the job histories: each successful docker-service update deployed an
  image, the push-manifest job that published it names the built image, and
  the build that produced that names the commit. The deployer rebuilds and
  redeploys daily, so deploys are not merges. When the branch's latest build
  produced the image already deployed, its commit counts as live too.
- GitHub gives the branch head, the commits since the live commit, and the
  merged PRs; a rebase merge's commits are grouped under their PR.
- On a terminal, `⏎` on a PR opens its stages and their logs: the current
  deploy for a PR not live yet, or the deploy that made it live.

### Feeds: scheduled and bot workflows

Above the PRs, `oeil repo` lists the repository's *feeds*: the GitHub
Actions workflows that run on their own, on a schedule, or that open PRs
from a bot branch, such as ocaml.org's scrapers (planet, platform releases,
opam-health) or its attribution sync. For each feed it shows its last run,
what that run did (*no change*, *opened #N*, *updated #N*, or *failed*),
where its latest PR is now (open, waiting, deploying, live), when it runs
next (from its cron schedule), and a strip of the last 14 days:

```
feeds
╭───┬───┬────────────┬─────────┬────────────┬────────────┬─────────┬─────────╮
│   │   │ feed       │ last    │ outcome    │ its PR     │    next │ 7 days  │
├───┼───┼────────────┼─────────┼────────────┼────────────┼─────────┼─────────┤
│   │ ✗ │ CI         │ 11h ago │ failed     │            │   in 6d │ ······✗ │
│   │ ✔ │ Scrape op… │ 1h ago  │ no change  │ #4011 live │  in 22h │ ○○●○○○○ │
│   │ ✔ │ Scrape pl… │ 7h ago  │ no change  │ #4015 live │  in 16h │ ●●●●●●○ │
│   │ ✔ │ Sync attr… │ 2h ago  │ no change  │ #4020 live │ on push │ ······● │
╰───┴───┴────────────┴─────────┴────────────┴────────────┴─────────┴─────────╯
  ● opened or updated a PR  ○ no change  ✗ failed  ✔ ran  · no run
```

(80 columns; a wider terminal also shows the schedule and 14 days.) What a
run did is read from timestamps: a bot PR created while the run ran was
opened by it, one that got a commit while it ran was updated by it.

### The life of a change

`⏎` on a PR, or on a feed (its latest PR), opens its timeline, from what
started it to production:

```
ocaml/ocaml.org · Sync attribution → #4020   ✔ live
│ ✔ │ 10:07:16 │ Sync attribution run                     │ push · opened #4020    │  5m21s │
│ ✔ │ 10:07:19 │   sync                                   │                        │  5m17s │
│ ✔ │ 10:12:30 │ PR #4020 opened                          │ by github-actions[bot] │     4s │
│ ✔ │ 10:12:34 │ CI checks                                │ 4 checks               │ 22m51s │
│ ✔ │ 10:12:34 │   deployability                          │ deploy-ci-ocaml-org    │ 14m38s │
│ ✔ │ 10:24:04 │   Build and test (ubuntu-latest, 5.5.2)  │                        │  5m06s │
│ ✔ │ 10:35:25 │ review and merge                         │ merged by cuihtlauac   │ 54m33s │
│ ✔ │ 11:29:58 │ merged                                   │ main @ 1ee8711f6       │     4s │
│ ✔ │ 11:30:02 │ deploy                                   │ deploy.ci.ocaml.org    │        │
│ ✔ │ 11:30:02 │   build linux-x86_64                     │                        │ 31m31s │
│ ✔ │ 12:02:41 │   service infra_www1                     │                        │        │
│ ✔ │ 12:02:41 │ live                                     │                        │        │
1h55m from the first step to the last · 54m33s (47%) waiting for review and merge
```

(2026-11-16, abridged.) `⏎` on a step shows its log: a GitHub Actions job's
log, or a deployer job's, followed live while it runs.

The first run reads several days of job logs (about 20–50 s for ocaml.org).
Finished jobs never change, so what was read is cached in
`~/.cache/oeil`, and later runs take a few seconds. Without a GitHub token,
`oeil repo` refreshes every 5 minutes instead of every 20 s, to stay within
GitHub's unauthenticated rate limit.

## Install

Needs OCaml ≥ 5.4.

```
opam install . --deps-only --with-test
dune build
dune install   # or: dune exec -- oeil …
```

## Commands

| Command | What it does |
|---|---|
| `oeil watch owner/repo#PR` | Waits for the PR to be merged, then follows its merge commit to the deploy target of the PR's base branch. |
| `oeil watch owner/repo@SHA [--branch B]` | Follows a commit (default branch `main`). |
| `oeil watch owner/repo:branch` | Follows the branch's latest build. |
| `oeil repo owner/repo [--recent H] [--once]` | Active PRs of each deployed branch and their deploy state; interactive on a terminal. |
| `oeil status [--repo owner/repo] [--all]` | Lists failed and active nodes with their errors. `--repo` also shows that repository's deployment chains. |
| `oeil log JOB [-f]` | Prints a job's log, or follows it while it runs. |
| `oeil mcp` | MCP server on stdin/stdout. |

`--instance URL` selects another OCurrent service (default
`https://deploy.ci.ocaml.org`). `watch` exits with 0 when the commit is deployed,
1 when the deploy failed, 2 on timeout (`--timeout`, default 1 h), 3 on a
bad target or an unreachable service, and 4 when you quit the interactive
view before the deployment ended.

GitHub is used to resolve a PR to its merge commit, and to check whether a
newer commit that got deployed instead includes yours. A token is taken from
`GITHUB_TOKEN`, `GH_TOKEN` or `gh auth token`. Without one, unauthenticated
API limits apply.

## MCP

```
claude mcp add oeil -- oeil mcp
```

Tools:

- `deploy_status {target}`: the status of each stage and the overall verdict
  (`in_progress`, `deployed`, `failed`).
- `wait_for_deploy {target, until?: "done"|"change", timeout_s?}`: blocks until
  the deploy finishes, or until the next stage change. It sends progress
  notifications when the client passes a progress token.
- `repo_status {repo, recent_hours?}`: the deploy state of a repository's
  merged PRs, per deployed branch, with the live commit and the build in
  flight, and its feeds with their recent runs and next run.
- `pr_timeline {repo, pr}`: a PR's life step by step, with times and
  durations: the bot run that opened it, its checks, the wait for review,
  the deployer's stages, when it went live.
- `pipeline_status {repo?}`: failed and active nodes, with error messages.
- `job_log {job, tail?}`.

## How it follows a commit

1. The build job's log names the commit it built
   (`…#refs/heads/main (<sha>)`) and the digest it pushed. If a newer commit
   was built instead, GitHub's compare API tells whether it includes yours.
   If it does, oeil follows that one and reports the target as superseded.
2. The push-manifest job's log names the built digest and the digest it
   published.
3. The pull and docker-service jobs name the published digest.
4. A stage whose matching job is older than the build is reported as
   *unchanged*: the image was already live, so nothing was redeployed.

The state of each node comes from its fill colour in the SVG. A two-colour
fill means a job is re-running while the node keeps its previous value.
A running job's page stays open and keeps sending the log, which is how
logs are followed live.

## License

ISC
