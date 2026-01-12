from pathlib import Path

root = Path(__file__).parent
assets = root / "assets"
dist = root / "dist"
service_worker_in = assets / "service-worker.js.in"
service_worker_out = dist / "service-worker.js"

# Collect all files in the assets directory
all_files = assets.glob("**/*")
all_files = [
    f.relative_to(assets)
    for f in all_files
    if f.is_file() and not f.name.startswith(".") and not f.name.endswith(".in")
]

# Prioritize critical files first
priority_files = []
other_files = []

for f in all_files:
    name = f.name.lower()
    # Critical files: HTML, CSS, JS, manifest
    if name.endswith(('.html', '.css', '.js', '.json')) and not name.startswith('icons'):
        priority_files.append(f)
    else:
        other_files.append(f)

# Sort each group
priority_files.sort(key=lambda f: (
    0 if f.name == 'index.html' else
    1 if f.name.endswith('.css') else
    2 if f.name == 'manifest.json' else
    3
))
other_files.sort()

# Combine: critical files first, then others
files = [f"            '{f.as_posix()}',\n" for f in priority_files + other_files]

# Add main.js at the very beginning (most critical)
if (dist / "main.js").exists():
    files.insert(0, "            'main.js',\n")

# Replace '__$ASSETS$__' in the service worker template
code = service_worker_in.read_text()
code = code.replace("            '__$ASSETS$__'", "".join(files))
service_worker_out.write_text(code)
