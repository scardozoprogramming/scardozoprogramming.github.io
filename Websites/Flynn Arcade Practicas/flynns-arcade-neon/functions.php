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
    echo '<link rel="icon" type="image/svg+xml" href="' . esc_url(get_template_directory_uri() . '/assets/images/favicon.svg') . '">' . "\n";
}
add_action('wp_head', 'flynns_arcade_neon_favicon');

