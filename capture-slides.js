#!/usr/bin/env node
/**
 * Capture screenshots of reveal.js title slides for PDF/EPUB versions
 *
 * Usage:
 *   npm install puppeteer (if not installed)
 *   node capture-slides.js
 */

const puppeteer = require('puppeteer');
const fs = require('fs');
const path = require('path');

// Mapping of narrative files to their slide URLs and output filenames
const slides = [
  { file: 'narrative_aov.qmd', url: 'https://dyerlabteaching.github.io/Analysis-of-Variance/slides.html#/title-slide', output: 'slides_aov.png' },
  { file: 'narrative_classic.qmd', url: 'https://dyerlabteaching.github.io/Graphics-That-Do-Not-Suck/slides_classic.html#/title-slide', output: 'slides_classic.png' },
  { file: 'narrative_containers.qmd', url: 'https://dyerlabteaching.github.io/Data-Containers/slides.html#/title-slide', output: 'slides_containers.png' },
  { file: 'narrative_correlation.qmd', url: 'https://dyerlabteaching.github.io/Correlation/slides.html#/title-slide', output: 'slides_correlation.png' },
  { file: 'narrative_datatypes.qmd', url: 'https://dyerlabteaching.github.io/Basic-Data-Types/slides.html#/title-slide', output: 'slides_datatypes.png' },
  { file: 'narrative_factors.qmd', url: 'https://dyerlabteaching.github.io/Factors/slides.html#/title-slide', output: 'slides_factors.png' },
  { file: 'narrative_ggplot.qmd', url: 'https://dyerlabteaching.github.io/Graphics-That-Do-Not-Suck/slides.html#/title-slide', output: 'slides_ggplot.png' },
  { file: 'narrative_joins.qmd', url: 'https://dyerlabteaching.github.io/Joins/slides.html#/title-slide', output: 'slides_joins.png' },
  { file: 'narrative_markdown.qmd', url: 'https://dyerlabteaching.github.io/Markdown/slides.html#/title-slide', output: 'slides_markdown.png' },
  { file: 'narrative_ordination.qmd', url: 'https://dyerlabteaching.github.io/Ordination/slides.html#/title-slide', output: 'slides_ordination.png' },
  { file: 'narrative_points.qmd', url: 'https://dyerlabteaching.github.io/Spatial-Points/slides.html#/title-slide', output: 'slides_points.png' },
  { file: 'narrative_rasters.qmd', url: 'https://dyerlabteaching.github.io/Raster-Data/slides.html#/title-slide', output: 'slides_rasters.png' },
  { file: 'narrative_regression.qmd', url: 'https://dyerlabteaching.github.io/Regression/slides.html#/title-slide', output: 'slides_regression.png' },
  { file: 'narrative_tidyverse.qmd', url: 'https://dyerlabteaching.github.io/Tidyverse/slides.html#/title-slide', output: 'slides_tidyverse.png' },
  { file: 'narrative_vector.qmd', url: 'https://dyerlabteaching.github.io/Shapefiles/slides.html#/title-slide', output: 'slides_vector.png' },
];

const MEDIA_DIR = path.join(__dirname, 'media');

async function captureSlide(browser, slide) {
  const page = await browser.newPage();

  // Set viewport to a reasonable size for slides (16:9 aspect ratio)
  await page.setViewport({ width: 1280, height: 720 });

  try {
    console.log(`Capturing: ${slide.file} -> ${slide.output}`);

    // Navigate to the slide URL
    await page.goto(slide.url, {
      waitUntil: 'networkidle2',
      timeout: 30000
    });

    // Wait a bit for any animations to complete
    await page.evaluate(() => new Promise(resolve => setTimeout(resolve, 1500)));

    // Take screenshot
    const outputPath = path.join(MEDIA_DIR, slide.output);
    await page.screenshot({
      path: outputPath,
      type: 'png'
    });

    console.log(`  ✓ Saved: ${outputPath}`);
    return true;
  } catch (error) {
    console.error(`  ✗ Error capturing ${slide.file}: ${error.message}`);
    return false;
  } finally {
    await page.close();
  }
}

async function main() {
  // Ensure media directory exists
  if (!fs.existsSync(MEDIA_DIR)) {
    fs.mkdirSync(MEDIA_DIR, { recursive: true });
  }

  console.log('Starting slide capture...\n');
  console.log(`Output directory: ${MEDIA_DIR}\n`);

  const browser = await puppeteer.launch({
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  let successful = 0;
  let failed = 0;

  for (const slide of slides) {
    const success = await captureSlide(browser, slide);
    if (success) successful++;
    else failed++;
  }

  await browser.close();

  console.log('\n--- Summary ---');
  console.log(`Successful: ${successful}`);
  console.log(`Failed: ${failed}`);
  console.log(`Total: ${slides.length}`);
}

main().catch(console.error);
