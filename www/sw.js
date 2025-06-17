// www/sw.js
// Service Worker for NUCLEARFF Mobile PWA

const CACHE_NAME = 'nuclearff-v1.0.0';
const STATIC_CACHE = 'nuclearff-static-v1.0.0';
const DYNAMIC_CACHE = 'nuclearff-dynamic-v1.0.0';

// Files to cache for offline functionality
const STATIC_FILES = [
  '/',
  '/app.js',
  '/mobile_styles.css',
  '/manifest.json',
  '/nuclearff/nuclearff-navbar-icon-color.png',
  '/nuclearff/nuclearff-navbar-icon-dark.png',
  // Framework7 and Shiny dependencies will be cached dynamically
];

// API endpoints that should be cached
const API_CACHE_PATTERNS = [
  /\/session\/[a-zA-Z0-9]+\/dataobj/,
  /\/session\/[a-zA-Z0-9]+\/file/,
  // Add other Shiny session patterns as needed
];

// Install event - cache static assets
self.addEventListener('install', event => {
  console.log('NUCLEARFF Service Worker: Installing...');
  
  event.waitUntil(
    caches.open(STATIC_CACHE)
      .then(cache => {
        console.log('NUCLEARFF Service Worker: Caching static files');
        return cache.addAll(STATIC_FILES);
      })
      .then(() => {
        console.log('NUCLEARFF Service Worker: Static files cached successfully');
        return self.skipWaiting(); // Activate immediately
      })
      .catch(error => {
        console.error('NUCLEARFF Service Worker: Failed to cache static files:', error);
      })
  );
});

// Activate event - clean up old caches
self.addEventListener('activate', event => {
  console.log('NUCLEARFF Service Worker: Activating...');
  
  event.waitUntil(
    caches.keys()
      .then(cacheNames => {
        return Promise.all(
          cacheNames.map(cacheName => {
            // Delete old cache versions
            if (cacheName !== STATIC_CACHE && 
                cacheName !== DYNAMIC_CACHE && 
                cacheName.startsWith('nuclearff-')) {
              console.log('NUCLEARFF Service Worker: Deleting old cache:', cacheName);
              return caches.delete(cacheName);
            }
          })
        );
      })
      .then(() => {
        console.log('NUCLEARFF Service Worker: Activated successfully');
        return self.clients.claim(); // Take control of all pages
      })
  );
});

// Fetch event - serve cached content or fetch from network
self.addEventListener('fetch', event => {
  const request = event.request;
  const url = new URL(request.url);
  
  // Skip non-GET requests
  if (request.method !== 'GET') {
    return;
  }
  
  // Skip Chrome extension and other non-http requests
  if (!request.url.startsWith('http')) {
    return;
  }
  
  // Handle different types of requests
  if (isStaticAsset(request)) {
    event.respondWith(handleStaticAsset(request));
  } else if (isAPIRequest(request)) {
    event.respondWith(handleAPIRequest(request));
  } else if (isNavigationRequest(request)) {
    event.respondWith(handleNavigationRequest(request));
  } else {
    event.respondWith(handleOtherRequest(request));
  }
});

// Check if request is for a static asset
function isStaticAsset(request) {
  const url = new URL(request.url);
  const pathname = url.pathname;
  
  return pathname.match(/\.(css|js|png|jpg|jpeg|gif|svg|ico|woff|woff2|ttf)$/) ||
         STATIC_FILES.some(file => pathname.endsWith(file));
}

// Check if request is for API/data
function isAPIRequest(request) {
  return API_CACHE_PATTERNS.some(pattern => pattern.test(request.url)) ||
         request.url.includes('/session/') ||
         request.url.includes('/websocket');
}

// Check if request is for navigation (page request)
function isNavigationRequest(request) {
  return request.mode === 'navigate' || 
         (request.method === 'GET' && request.headers.get('accept').includes('text/html'));
}

// Handle static asset requests with cache-first strategy
async function handleStaticAsset(request) {
  try {
    // Try cache first
    const cachedResponse = await caches.match(request);
    if (cachedResponse) {
      console.log('NUCLEARFF Service Worker: Serving static asset from cache:', request.url);
      return cachedResponse;
    }
    
    // Fetch from network and cache
    const networkResponse = await fetch(request);
    if (networkResponse.ok) {
      const cache = await caches.open(STATIC_CACHE);
      cache.put(request, networkResponse.clone());
      console.log('NUCLEARFF Service Worker: Cached new static asset:', request.url);
    }
    
    return networkResponse;
    
  } catch (error) {
    console.error('NUCLEARFF Service Worker: Failed to fetch static asset:', error);
    
    // Return offline fallback for critical assets
    if (request.url.includes('.css')) {
      return new Response('/* Offline mode - styles unavailable */', {
        headers: { 'Content-Type': 'text/css' }
      });
    }
    
    // Return empty response for other assets
    return new Response('', { status: 200 });
  }
}

// Handle API requests with network-first strategy
async function handleAPIRequest(request) {
  try {
    // Try network first for fresh data
    const networkResponse = await fetch(request);
    
    if (networkResponse.ok) {
      // Cache successful responses
      const cache = await caches.open(DYNAMIC_CACHE);
      cache.put(request, networkResponse.clone());
      console.log('NUCLEARFF Service Worker: Cached API response:', request.url);
    }
    
    return networkResponse;
    
  } catch (error) {
    console.warn('NUCLEARFF Service Worker: Network failed for API request, trying cache:', error);
    
    // Fallback to cache
    const cachedResponse = await caches.match(request);
    if (cachedResponse) {
      console.log('NUCLEARFF Service Worker: Serving API response from cache:', request.url);
      // Add header to indicate cached response
      const response = cachedResponse.clone();
      response.headers.set('X-Served-From', 'cache');
      return response;
    }
    
    // Return offline indicator for API failures
    return new Response(JSON.stringify({
      error: 'offline',
      message: 'This data is not available offline'
    }), {
      status: 503,
      headers: { 'Content-Type': 'application/json' }
    });
  }
}

// Handle navigation requests
async function handleNavigationRequest(request) {
  try {
    // Try network first for fresh content
    const networkResponse = await fetch(request);
    
    if (networkResponse.ok) {
      // Cache the page
      const cache = await caches.open(DYNAMIC_CACHE);
      cache.put(request, networkResponse.clone());
    }
    
    return networkResponse;
    
  } catch (error) {
    console.warn('NUCLEARFF Service Worker: Network failed for navigation, serving cached version:', error);
    
    // Try to serve cached version
    const cachedResponse = await caches.match(request);
    if (cachedResponse) {
      return cachedResponse;
    }
    
    // Serve offline page
    return caches.match('/') || new Response(`
      <!DOCTYPE html>
      <html>
      <head>
        <title>NUCLEARFF - Offline</title>
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <style>
          body { 
            font-family: -apple-system, BlinkMacSystemFont, sans-serif;
            text-align: center; 
            padding: 50px 20px;
            background: #f5f5f5;
          }
          .offline-container {
            background: white;
            border-radius: 12px;
            padding: 40px 20px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            max-width: 400px;
            margin: 0 auto;
          }
          .offline-icon {
            font-size: 48px;
            margin-bottom: 20px;
          }
          .retry-btn {
            background: #007AFF;
            color: white;
            border: none;
            padding: 12px 24px;
            border-radius: 8px;
            font-size: 16px;
            cursor: pointer;
            margin-top: 20px;
          }
        </style>
      </head>
      <body>
        <div class="offline-container">
          <div class="offline-icon">📡</div>
          <h1>You're Offline</h1>
          <p>NUCLEARFF is currently unavailable. Please check your internet connection and try again.</p>
          <button class="retry-btn" onclick="window.location.reload()">Try Again</button>
        </div>
      </body>
      </html>
    `, {
      headers: { 'Content-Type': 'text/html' }
    });
  }
}

// Handle other requests with cache-first strategy
async function handleOtherRequest(request) {
  try {
    // Check cache first
    const cachedResponse = await caches.match(request);
    if (cachedResponse) {
      return cachedResponse;
    }
    
    // Fetch from network
    const networkResponse = await fetch(request);
    
    if (networkResponse.ok) {
      // Cache successful responses
      const cache = await caches.open(DYNAMIC_CACHE);
      cache.put(request, networkResponse.clone());
    }
    
    return networkResponse;
    
  } catch (error) {
    console.error('NUCLEARFF Service Worker: Request failed:', error);
    return new Response('Service Unavailable', { status: 503 });
  }
}

// Background sync for offline actions
self.addEventListener('sync', event => {
  console.log('NUCLEARFF Service Worker: Background sync triggered:', event.tag);
  
  if (event.tag === 'sync-league-data') {
    event.waitUntil(syncLeagueData());
  }
});

// Sync league data when back online
async function syncLeagueData() {
  try {
    // Get stored offline actions
    const offlineActions = await getOfflineActions();
    
    for (const action of offlineActions) {
      try {
        await fetch(action.url, action.options);
        console.log('NUCLEARFF Service Worker: Synced offline action:', action.type);
        
        // Remove from offline storage
        await removeOfflineAction(action.id);
        
      } catch (error) {
        console.error('NUCLEARFF Service Worker: Failed to sync action:', error);
      }
    }
    
  } catch (error) {
    console.error('NUCLEARFF Service Worker: Background sync failed:', error);
  }
}

// Store offline actions for later sync
async function storeOfflineAction(action) {
  try {
    const db = await openOfflineDB();
    const transaction = db.transaction(['offline_actions'], 'readwrite');
    const store = transaction.objectStore('offline_actions');
    await store.add(action);
    
  } catch (error) {
    console.error('NUCLEARFF Service Worker: Failed to store offline action:', error);
  }
}

// Get stored offline actions
async function getOfflineActions() {
  try {
    const db = await openOfflineDB();
    const transaction = db.transaction(['offline_actions'], 'readonly');
    const store = transaction.objectStore('offline_actions');
    return await store.getAll();
    
  } catch (error) {
    console.error('NUCLEARFF Service Worker: Failed to get offline actions:', error);
    return [];
  }
}

// Remove synced offline action
async function removeOfflineAction(id) {
  try {
    const db = await openOfflineDB();
    const transaction = db.transaction(['offline_actions'], 'readwrite');
    const store = transaction.objectStore('offline_actions');
    await store.delete(id);
    
  } catch (error) {
    console.error('NUCLEARFF Service Worker: Failed to remove offline action:', error);
  }
}

// Open IndexedDB for offline storage
function openOfflineDB() {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open('nuclearff_offline', 1);
    
    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);
    
    request.onupgradeneeded = (event) => {
      const db = event.target.result;
      
      if (!db.objectStoreNames.contains('offline_actions')) {
        const store = db.createObjectStore('offline_actions', { keyPath: 'id' });
        store.createIndex('timestamp', 'timestamp', { unique: false });
      }
    };
  });
}

// Push notification handling
self.addEventListener('push', event => {
  console.log('NUCLEARFF Service Worker: Push notification received');
  
  const options = {
    body: 'Your league data has been updated',
    icon: '/nuclearff/nuclearff-navbar-icon-color.png',
    badge: '/nuclearff/nuclearff-navbar-icon-color.png',
    vibrate: [200, 100, 200],
    data: {
      dateOfArrival: Date.now(),
      primaryKey: 1
    },
    actions: [
      {
        action: 'view',
        title: 'View Data',
        icon: '/icons/view.png'
      },
      {
        action: 'close',
        title: 'Close',
        icon: '/icons/close.png'
      }
    ]
  };
  
  if (event.data) {
    const data = event.data.json();
    options.body = data.body || options.body;
    options.title = data.title || 'NUCLEARFF Update';
  }
  
  event.waitUntil(
    self.registration.showNotification('NUCLEARFF', options)
  );
});

// Notification click handling
self.addEventListener('notificationclick', event => {
  console.log('NUCLEARFF Service Worker: Notification clicked');
  
  event.notification.close();
  
  if (event.action === 'view') {
    // Open the app
    event.waitUntil(
      clients.openWindow('/')
    );
  }
  // 'close' action just closes the notification
});

// Message handling from main app
self.addEventListener('message', event => {
  console.log('NUCLEARFF Service Worker: Message received:', event.data);
  
  if (event.data && event.data.type === 'SKIP_WAITING') {
    self.skipWaiting();
  }
  
  if (event.data && event.data.type === 'CACHE_LEAGUE_DATA') {
    // Cache specific league data
    event.waitUntil(
      caches.open(DYNAMIC_CACHE).then(cache => {
        return cache.put(event.data.url, new Response(JSON.stringify(event.data.data), {
          headers: { 'Content-Type': 'application/json' }
        }));
      })
    );
  }
});

// Periodic cleanup of old cache entries
self.addEventListener('activate', event => {
  event.waitUntil(
    cleanupOldCacheEntries()
  );
});

async function cleanupOldCacheEntries() {
  try {
    const cache = await caches.open(DYNAMIC_CACHE);
    const requests = await cache.keys();
    const now = Date.now();
    const maxAge = 24 * 60 * 60 * 1000; // 24 hours
    
    for (const request of requests) {
      const response = await cache.match(request);
      const dateHeader = response.headers.get('date');
      
      if (dateHeader) {
        const cacheDate = new Date(dateHeader).getTime();
        if (now - cacheDate > maxAge) {
          await cache.delete(request);
          console.log('NUCLEARFF Service Worker: Cleaned up old cache entry:', request.url);
        }
      }
    }
    
  } catch (error) {
    console.error('NUCLEARFF Service Worker: Cache cleanup failed:', error);
  }
}

console.log('NUCLEARFF Service Worker: Loaded successfully');