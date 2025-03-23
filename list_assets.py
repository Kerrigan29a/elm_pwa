from pathlib import Path

root = Path(__file__).parent
assets = root / 'assets'
dist = root / 'dist'
service_worker_in = assets / 'service-worker.js.in'
service_worker_out = dist / 'service-worker.js'

# Collect all files in the assets directory
files = assets.glob('**/*')
files = (f.relative_to(assets) for f in files)
files = (f"            '{f.as_posix()}',\n" for f in files)

# Replace '__$ASSETS$__' in the service worker template
code = service_worker_in.read_text()
code = code.replace("            '__$ASSETS$__'",
                    ''.join(files))
service_worker_out.write_text(code)
