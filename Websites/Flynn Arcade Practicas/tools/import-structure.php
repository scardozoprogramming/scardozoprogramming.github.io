<?php
/** Extract public source structure and an asset manifest without importing its scripts. */
$sourceDirectory = 'C:/Users/santi/AppData/Local/Temp/flynns-source-snapshots';
libxml_use_internal_errors(true);
function source_document(string $file): DOMXPath {
    $document = new DOMDocument();
    $document->loadHTML('<?xml encoding="UTF-8">' . file_get_contents($file));
    return new DOMXPath($document);
}
function source_text(DOMNode $node): string {
    return trim(preg_replace('/\s+/u', ' ', $node->textContent));
}
function source_slug(string $text): string {
    return trim(strtolower(preg_replace('/[^a-zA-Z0-9]+/', '-', $text)), '-');
}
function source_image_url(DOMElement $image): string {
    $choices = array();
    foreach (explode(',', $image->getAttribute('srcset')) as $candidate) {
        $parts = preg_split('/\s+/', trim($candidate));
        if (count($parts) === 2 && intval($parts[1]) >= 640 && intval($parts[1]) <= 1024) $choices[intval($parts[1])] = $parts[0];
    }
    if ($choices) { ksort($choices); return reset($choices); }
    return $image->getAttribute('src');
}
$assets = array();
function source_asset(string $url, string $base): string {
    global $assets;
    $extension = pathinfo(parse_url($url, PHP_URL_PATH), PATHINFO_EXTENSION) ?: 'png';
    $filename = $base . '.' . $extension;
    $assets[$filename] = $url;
    return $filename;
}
$home = source_document($sourceDirectory . '/home.html');
$logo = $home->query('//*[@data-elementor-type="header"]//img')->item(0);
$result = array('logo' => source_asset(source_image_url($logo), 'brand/flynns-logo-original'), 'studios' => array(), 'press' => array(), 'presskits' => array());
$favicon = $home->query('//link[@rel="icon"]')->item(0);
if ($favicon) $result['favicon'] = source_asset($favicon->getAttribute('href'), 'brand/favicon-original');

$indies = source_document($sourceDirectory . '/indies.html');
foreach ($indies->query('//section[.//h3 and not(.//section[.//h3])]') as $section) {
    $heading = $indies->query('.//h3', $section)->item(0);
    if (!$heading) continue;
    $name = source_text($heading);
    $slug = source_slug($name);
    $images = array();
    foreach ($indies->query('.//img', $section) as $image) {
        $url = $image->getAttribute('src');
        if (strpos($url, '/wp-content/uploads/') !== false) $images[] = source_image_url($image);
    }
    $paragraphs = array();
    foreach ($indies->query('.//p', $section) as $paragraph) { if (source_text($paragraph)) $paragraphs[] = source_text($paragraph); }
    $website = $indies->query('.//h4//a', $section)->item(0);
    $result['studios'][] = array('name' => $name, 'logo' => !empty($images[0]) ? source_asset($images[0], 'studios/' . $slug . '-logo') : '', 'portrait' => !empty($images[1]) ? source_asset($images[1], 'studios/' . $slug . '-portrait') : '', 'website' => $website ? $website->getAttribute('href') : '', 'paragraphs' => $paragraphs);
}
$press = source_document($sourceDirectory . '/press.html');
foreach ($press->query('//section[.//h3 and not(.//section[.//h3])]') as $section) {
    $heading = $press->query('.//h3', $section)->item(0);
    if (!$heading || strpos(source_text($heading), 'Press Media') !== false) continue;
    $image = $press->query('.//img', $section)->item(0);
    $links = $press->query('.//a', $section);
    $article = '';
    foreach ($links as $link) {
        if (stripos(source_text($link), 'READ MORE') !== false) $article = $link->getAttribute('href');
    }
    $texts = array();
    foreach ($press->query('.//p', $section) as $paragraph) { if (source_text($paragraph)) $texts[] = source_text($paragraph); }
    $result['press'][] = array('heading' => source_text($heading), 'image' => $image ? source_asset(source_image_url($image), 'press/review-' . count($result['press'])) : '', 'article_url' => $article, 'paragraphs' => $texts);
}
$about = source_document($sourceDirectory . '/about.html');
foreach ($press->query('//a[contains(@href, "drive.google.com")]') as $link) {
    $column = $press->query('ancestor::*[contains(concat(" ", normalize-space(@class), " "), " elementor-column ")][1]', $link)->item(0);
    $image = $column ? $press->query('.//img', $column)->item(0) : null;
    if ($image) $result['presskits'][] = array('image' => source_asset(source_image_url($image), 'press/presskit-' . count($result['presskits'])), 'label' => $image->getAttribute('alt'));
}
$aboutImage = $about->query('//*[@data-elementor-type="wp-page"]//img')->item(0);
if ($aboutImage) $result['about_image'] = source_asset($aboutImage->getAttribute('src'), 'brand/about-original');
$promotions = array(
    'eternum' => 'https://www.flynnsarcades.com/wp-content/uploads/2021/07/EternumEx_asia.png',
    'dexter' => 'https://www.flynnsarcades.com/wp-content/uploads/2022/01/Dexter_solo_slide-copia.png',
    'wca' => 'https://www.flynnsarcades.com/wp-content/uploads/2021/07/Wca_cover.png',
);
foreach ($promotions as $key => $url) $result['promotions'][$key] = source_asset($url, 'promotions/' . $key);
file_put_contents($sourceDirectory . '/structure.json', json_encode($result, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE));
file_put_contents($sourceDirectory . '/assets.json', json_encode($assets, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES));
// Runtime data contains only local asset paths and independent article links.
$runtime = $result;
foreach ($runtime['studios'] as &$studio) unset($studio['paragraphs']);
unset($studio);
foreach ($runtime['press'] as &$review) unset($review['paragraphs']);
unset($review);
file_put_contents(dirname(__DIR__) . '/flynns-arcade-neon/inc/structure.json', json_encode($runtime, JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE) . "\n");
echo json_encode(array('logo' => $result['logo'], 'studios' => array_map(function ($studio) { return array('name' => $studio['name'], 'logo' => $studio['logo'], 'portrait' => $studio['portrait']); }, $result['studios']), 'press' => $result['press'], 'about_image' => $result['about_image'] ?? ''), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE);
