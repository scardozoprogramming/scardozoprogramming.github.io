<?php
/** Generate every static page from the same templates used by WordPress. */
define('ABSPATH', __DIR__);
define('FLYNNS_STATIC_PREVIEW', true);
define('FLYNNS_THEME', dirname(__DIR__) . '/flynns-arcade-neon');
$previewQuery = array();
function get_template_directory() { return FLYNNS_THEME; }
function get_theme_file_uri($path) { return $path; }
function get_query_var($key, $default = '') { global $previewQuery; return $previewQuery[$key] ?? $default; }
function home_url($path = '/') { return 'preview.html'; }
function __($text, $domain = '') { return $text; }
function esc_html($text) { return htmlspecialchars((string) $text, ENT_QUOTES, 'UTF-8'); }
function esc_attr($text) { return esc_html($text); }
function esc_url($text) { return esc_html($text); }
function esc_html__($text, $domain = '') { return esc_html($text); }
function esc_attr__($text, $domain = '') { return esc_attr($text); }
function esc_html_e($text, $domain = '') { echo esc_html($text); }
function esc_attr_e($text, $domain = '') { echo esc_attr($text); }
function sanitize_html_class($text) { return preg_replace('/[^a-zA-Z0-9_-]/', '', $text); }
function wp_json_encode($value, $flags = 0) { return json_encode($value, $flags); }
function language_attributes() { echo 'lang="en"'; }
function bloginfo($name) { echo 'utf-8'; }
function body_class() {}
function wp_body_open() {}
function wp_head() {
    $view = flynns_arcade_neon_view();
    $title = 'game' === $view ? flynns_arcade_neon_current_game()['title'] : ('home' === $view ? 'Inicio' : ucfirst($view));
    echo '<title>' . esc_html($title) . ' | Flynn\'s Arcade</title><link rel="stylesheet" href="style.css?v=2.4.0"><link rel="icon" href="' . esc_url(flynns_arcade_neon_asset(flynns_arcade_neon_data('structure')['favicon'])) . '">';
}
function wp_footer() { echo '<script src="assets/js/theme.js?v=2.4.0"></script>'; }
function get_header() { require FLYNNS_THEME . '/header.php'; }
function get_footer() { require FLYNNS_THEME . '/footer.php'; }
require FLYNNS_THEME . '/inc/content.php';
require FLYNNS_THEME . '/inc/template-tags.php';
$pages = array('preview.html' => array('flynns_view' => 'home'), 'games.html' => array('flynns_view' => 'games'), 'indies.html' => array('flynns_view' => 'indies'), 'about.html' => array('flynns_view' => 'about'), 'press.html' => array('flynns_view' => 'press'));
foreach (flynns_arcade_neon_games() as $game) {
    $slug = flynns_arcade_neon_slug($game['title']);
    $pages['game-' . $slug . '.html'] = array('flynns_view' => 'game', 'flynns_game' => $slug);
}
foreach ($pages as $file => $query) {
    $previewQuery = $query;
    ob_start();
    require FLYNNS_THEME . '/front-page.php';
    file_put_contents(FLYNNS_THEME . '/' . $file, ob_get_clean());
}
copy(FLYNNS_THEME . '/preview.html', FLYNNS_THEME . '/index.html');
echo count($pages) . " static pages generated from the WordPress templates.\n";
