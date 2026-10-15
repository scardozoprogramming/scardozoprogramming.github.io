<?php
/**
 * Flynn's Arcade Neon theme functions.
 *
 * @package FlynnsArcadeNeon
 */

if (!defined('ABSPATH')) {
    exit;
}

require_once get_template_directory() . '/inc/content.php';
require_once get_template_directory() . '/inc/template-tags.php';

function flynns_arcade_neon_setup(): void {
    add_theme_support('title-tag');
    add_theme_support('post-thumbnails');
    add_theme_support('html5', array('search-form', 'gallery', 'caption', 'style', 'script'));

}
add_action('after_setup_theme', 'flynns_arcade_neon_setup');

function flynns_arcade_neon_assets(): void {
    $theme = wp_get_theme();
    $version = $theme->get('Version');

    wp_enqueue_style(
        'flynns-arcade-neon-style',
        get_stylesheet_uri(),
        array(),
        $version
    );

    wp_enqueue_script(
        'flynns-arcade-neon-theme',
        get_template_directory_uri() . '/assets/js/theme.js',
        array(),
        $version,
        true
    );
}
add_action('wp_enqueue_scripts', 'flynns_arcade_neon_assets');

function flynns_arcade_neon_favicon(): void {
    echo '<link rel="icon" href="' . esc_url(flynns_arcade_neon_asset(flynns_arcade_neon_data('structure')['favicon'])) . '">' . "\n";
}
add_action('wp_head', 'flynns_arcade_neon_favicon');

function flynns_arcade_neon_query_vars(array $vars): array {
    $vars[] = 'flynns_view';
    $vars[] = 'flynns_game';
    return $vars;
}
add_filter('query_vars', 'flynns_arcade_neon_query_vars');

function flynns_arcade_neon_document_title(string $title): string {
    $view = flynns_arcade_neon_view();
    if ('game' === $view) return flynns_arcade_neon_current_game()['title'] . " | Flynn's Arcade";
    return ('home' === $view ? 'Inicio' : ucfirst($view)) . " | Flynn's Arcade";
}
add_filter('pre_get_document_title', 'flynns_arcade_neon_document_title');

