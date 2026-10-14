<?php
/** Inspect the public reference snapshots using the HTML parser. */
$directory = getenv('TEMP') . '/flynns-source-snapshots';
libxml_use_internal_errors(true);
function document(string $html): DOMXPath {
    $dom = new DOMDocument();
    $dom->loadHTML('<?xml encoding="UTF-8">' . $html);
    return new DOMXPath($dom);
}
function text(DOMNode $node): string {
    return trim(preg_replace('/\s+/u', ' ', $node->textContent));
}
if (($argv[1] ?? '') === 'details') {
    foreach (glob($directory . '/*.html') as $file) {
        if (in_array(basename($file), array('home.html', 'indies.html', 'about.html', 'press.html'), true)) continue;
        $xpath = document(file_get_contents($file));
        $main = $xpath->query('//*[@data-elementor-type="wp-page"]')->item(0);
        if (!$main) continue;
        $lines = array();
        foreach ($xpath->query('.//h2 | .//p', $main) as $node) {
            if (text($node)) $lines[] = text($node);
        }
        echo basename($file) . "\n" . implode("\n", array_unique($lines)) . "\n\n";
    }
    exit;
}
$home = document(file_get_contents($directory . '/home.html'));
$items = $home->query('//*[contains(concat(" ", normalize-space(@class), " "), " eael-filterable-gallery-item-wrap ")]');
$fragments = array();
foreach ($items as $item) { $fragments[] = $item->ownerDocument->saveHTML($item); }
foreach ($home->query('//*[@*]') as $element) {
    foreach ($element->attributes as $attribute) {
        if (strlen($attribute->value) < 5000) { continue; }
        $data = json_decode(base64_decode($attribute->value, true) ?: '', true);
        if (is_array($data)) {
            echo 'Encoded gallery attribute: ' . $attribute->name . ' / ' . count($data) . " items\n";
            foreach ($data as $entry) { if (is_string($entry)) $fragments[] = $entry; }
        }
    }
}
$games = array();
foreach ($fragments as $fragment) {
    $xpath = document($fragment);
    $heading = $xpath->query('//*[contains(@class, "fg-item-title")]')->item(0);
    if (!$heading) continue;
    $title = text($heading);
    if (isset($games[$title])) continue;
    $image = $xpath->query('//*[contains(@class, "gallery-item-thumbnail-wrap")]//img')->item(0);
    $caption = $xpath->query('//*[contains(@class, "fg-item-content")]')->item(0);
    $intro = $xpath->query('.//p[1]', $caption)->item(0);
    $link = $xpath->query('.//p[1]//a', $caption)->item(0);
    $category = $xpath->query('//*[@data-search-categories]')->item(0);
    $platforms = array();
    foreach ($xpath->query('.//img', $caption) as $icon) {
        $src = strtolower($icon->getAttribute('src'));
        $href = $icon->parentNode instanceof DOMElement ? $icon->parentNode->getAttribute('href') : '';
        if (strpos($src, 'nintendo') !== false || strpos($href, 'nintendo') !== false) $platforms[] = 'Switch';
        elseif (strpos($src, 'steam') !== false || strpos($href, 'steampowered') !== false) $platforms[] = 'PC';
        elseif (strpos($src, 'playstation') !== false || strpos($src, 'sony') !== false || strpos($href, 'playstation') !== false) $platforms[] = 'PlayStation';
        elseif (strpos($src, 'xbox') !== false || strpos($href, 'xbox') !== false) $platforms[] = 'Xbox';
    }
    $imageUrl = $image->getAttribute('src');
    foreach (explode(',', $image->getAttribute('srcset')) as $candidate) {
        $parts = preg_split('/\s+/', trim($candidate));
        if (count($parts) === 2 && intval($parts[1]) >= 600 && intval($parts[1]) <= 1024) {
            $imageUrl = $parts[0];
        }
    }
    $games[$title] = array('title' => $title, 'summary' => $intro ? text($intro) : '', 'categories' => $category->getAttribute('data-search-categories'), 'platforms' => array_values(array_unique($platforms)), 'source' => $link ? $link->getAttribute('href') : '', 'image' => $imageUrl);
}
file_put_contents($directory . '/catalog-source.json', json_encode(array_values($games), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE));
echo json_encode(array_values($games), JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE) . "\n";
$indies = document(file_get_contents($directory . '/indies.html'));
echo "STUDIOS\n";
foreach ($indies->query('//section[.//h3]') as $section) {
    $heading = $indies->query('.//h3', $section)->item(0);
    if (!$heading) continue;
    $snippets = array();
    foreach ($indies->query('.//*[contains(@class, "elementor-widget-text-editor")]//p', $section) as $paragraph) { if (text($paragraph)) $snippets[] = text($paragraph); }
    echo text($heading) . ': ' . implode(' / ', $snippets) . "\n";
}
