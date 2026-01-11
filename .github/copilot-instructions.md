## Commit Message Guidelines

When generating commit messages for this repository:

- Follow the classic Git commit style (human-readable, not machine-parsed).
- Write the subject line in **imperative mood** (e.g., "Add", "Fix", "Refactor").
  The message should complete: "If applied, this commit will …".
- Keep the first line concise and descriptive (aim for ~50 chars, hard limit 72).
- Do NOT end the subject line with a period.
- Do NOT use Conventional Commits or structured prefixes (`feat:`, `fix:`, etc.).
- Avoid vague messages ("update stuff", "misc changes").

Body (optional, but encouraged when useful):
- Separate subject and body with a blank line.
- Use the body to explain **why** the change was made and any relevant context.
- Prefer intent and impact over listing files or mechanical changes.
- Wrap body lines at ~72 characters when possible.

General rules:
- If multiple unrelated changes are present, suggest splitting into multiple commits.
- If changes are closely related, summarize the dominant intent.
- Output ONLY the commit message text (no explanations, no markdown).
