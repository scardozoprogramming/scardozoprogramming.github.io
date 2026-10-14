<?php
/** Render the actual theme templates without a WordPress installation. */
define('ABSPATH', __DIR__);
define('FLYNNS_THEME', dirname(__DIR__) . '/flynns-arcade-neon');
function get_template_directory() { return FLYNNS_THEME; }
function get_theme_file_uri($path) { return $path; }
function home_url($path = '/') { return '#top'; }
function __($text, $domain = '') { return $text; }
function esc_html($text) { return htmlspecialchars($text, ENT_QUOTES, 'UTF-8'); }
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
    echo '<title>Flynn\'s Arcade Publishing</title><link rel="stylesheet" href="style.css"><link rel="icon" href="assets/images/favicon.svg" type="image/svg+xml">';
}
function wp_footer() { echo '<script src="assets/js/theme.js"></script>'; }
function get_header() { require FLYNNS_THEME . '/header.php'; }
function get_footer() { require FLYNNS_THEME . '/footer.php'; }
require FLYNNS_THEME . '/inc/content.php';
require FLYNNS_THEME . '/inc/template-tags.php';
ob_start();
require FLYNNS_THEME . '/front-page.php';
$html = ob_get_clean();
file_put_contents(FLYNNS_THEME . '/preview.html', $html);
echo "Preview generated from the WordPress templates.\n";
