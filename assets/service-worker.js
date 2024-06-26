/*
Sources:
- https://developers.google.com/web/fundamentals/primers/service-workers/
- https://github.com/mdn/pwa-examples
- https://developer.mozilla.org/es/docs/Web/Progressive_web_apps
*/

var CACHE_NAME = 'dice-roller-v1';

self.addEventListener('install', (event) => {
    console.log('[Service Worker] Install');
    event.waitUntil(
        caches.open(CACHE_NAME).then((cache) => cache.addAll([
            'index.html',
            'main.js',
            'manifest.json',
            'style.css',
        ])),
    );
});

self.addEventListener('fetch', (event) => {
    event.respondWith(
        caches.match(event.request).then((response) => {
            console.log('[Service Worker] Fetching: ' + event.request.url);
            return response || fetch(event.request).then((response) => {
                return caches.open(CACHE_NAME).then((cache) => {
                    console.log('[Service Worker] Catching: ' + event.request.url);
                    cache.put(event.request, response.clone());
                    return response;
                });
            });
        })
    );
});
