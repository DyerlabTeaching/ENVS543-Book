#!/usr/bin/env node
/**
 * Capture screenshots of interactive widgets (leaflet maps) from rendered HTML
 * Starts a local server to avoid file:// security restrictions
 */

const puppeteer = require('puppeteer');
const path = require('path');
const fs = require('fs');
const http = require('http');

const DOCS_DIR = path.join(__dirname, 'docs');
const MEDIA_DIR = path.join(__dirname, 'media');
const PORT = 8765;

// Widget captures
const widgets = [
  {
    htmlFile: 'narrative_points.html',
    selector: '.leaflet',
    altSelectors: ['.leaflet-container', '.html-widget', '[id^="htmlwidget-"]'],
    output: 'map_points_leaflet.png',
    description: 'Beetle sampling sites map'
  },
  {
    htmlFile: 'narrative_containers.html',
    selector: '.leaflet',
    altSelectors: ['.leaflet-container', '.html-widget', '[id^="htmlwidget-"]'],
    output: 'map_containers_leaflet.png',
    description: 'Beetles location map'
  }
];

// Simple static file server
function startServer() {
  return new Promise((resolve) => {
    const server = http.createServer((req, res) => {
      let filePath = path.join(DOCS_DIR, req.url === '/' ? 'index.html' : req.url);

      // Handle paths without extension
      if (!path.extname(filePath) && !filePath.endsWith('/')) {
        filePath += '.html';
      }

      const ext = path.extname(filePath).toLowerCase();
      const mimeTypes = {
        '.html': 'text/html',
        '.js': 'application/javascript',
        '.css': 'text/css',
        '.json': 'application/json',
        '.png': 'image/png',
        '.jpg': 'image/jpeg',
        '.gif': 'image/gif',
        '.svg': 'image/svg+xml',
        '.woff': 'font/woff',
        '.woff2': 'font/woff2'
      };

      fs.readFile(filePath, (err, content) => {
        if (err) {
          res.writeHead(404);
          res.end('Not found');
        } else {
          res.writeHead(200, { 'Content-Type': mimeTypes[ext] || 'application/octet-stream' });
          res.end(content);
        }
      });
    });

    server.listen(PORT, () => {
      console.log(`Local server started on http://localhost:${PORT}`);
      resolve(server);
    });
  });
}

async function captureWidget(browser, widget) {
  const page = await browser.newPage();
  await page.setViewport({ width: 1200, height: 900 });

  const url = `http://localhost:${PORT}/${widget.htmlFile}`;

  try {
    console.log(`\nCapturing: ${widget.description}`);
    console.log(`  Source: ${widget.htmlFile}`);

    await page.goto(url, {
      waitUntil: 'networkidle2',
      timeout: 60000
    });

    // Try multiple selectors to find the widget
    let element = null;
    const allSelectors = [widget.selector, ...widget.altSelectors];

    for (const sel of allSelectors) {
      try {
        await page.waitForSelector(sel, { timeout: 5000 });
        const elements = await page.$$(sel);
        if (elements.length > 0) {
          // Get the last one (the map we want is usually later in the document)
          element = elements[elements.length - 1];
          console.log(`  Found widget with selector: ${sel}`);
          break;
        }
      } catch (e) {
        // Try next selector
      }
    }

    if (!element) {
      throw new Error('Could not find widget with any selector');
    }

    // Wait for map tiles to load - leaflet tiles load asynchronously
    console.log('  Waiting for map tiles to load...');
    await page.evaluate(() => new Promise(resolve => setTimeout(resolve, 8000)));

    // Scroll the widget into view to trigger tile loading
    await element.scrollIntoView();
    await page.evaluate(() => new Promise(resolve => setTimeout(resolve, 3000)));

    const outputPath = path.join(MEDIA_DIR, widget.output);
    await element.screenshot({ path: outputPath, type: 'png' });

    console.log(`  ✓ Saved: ${outputPath}`);
    return true;
  } catch (error) {
    console.error(`  ✗ Error: ${error.message}`);
    return false;
  } finally {
    await page.close();
  }
}

async function main() {
  if (!fs.existsSync(MEDIA_DIR)) {
    fs.mkdirSync(MEDIA_DIR, { recursive: true });
  }

  if (!fs.existsSync(DOCS_DIR)) {
    console.error('Error: docs/ directory not found.');
    process.exit(1);
  }

  console.log('Capturing interactive widgets...');

  const server = await startServer();

  const browser = await puppeteer.launch({
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  let successful = 0;
  let failed = 0;

  for (const widget of widgets) {
    const success = await captureWidget(browser, widget);
    if (success) successful++;
    else failed++;
  }

  await browser.close();
  server.close();

  console.log('\n--- Summary ---');
  console.log(`Successful: ${successful}`);
  console.log(`Failed: ${failed}`);
}

main().catch(console.error);
