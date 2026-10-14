<?php
/**
 * Site header.
 *
 * @package FlynnsArcadeNeon
 */

if (!defined('ABSPATH')) {
    exit;
}
?>
<!doctype html>
<html <?php language_attributes(); ?>>
<head>
    <meta charset="<?php bloginfo('charset'); ?>">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <?php wp_head(); ?>
</head>
<body <?php body_class(); ?>>
<?php wp_body_open(); ?>
<?php readfile(get_template_directory() . '/assets/icons/ui.svg'); ?>
<a class="skip-link screen-reader-text" href="#main-content"><?php esc_html_e('Skip to content', 'flynns-arcade-neon'); ?></a>
<div class="site-shell">
    <header class="site-header" id="top">
        <div class="header-inner">
            <?php flynns_arcade_neon_brand(); ?>

            <button class="nav-toggle" type="button" aria-label="<?php esc_attr_e('Open navigation', 'flynns-arcade-neon'); ?>" aria-controls="site-navigation" aria-expanded="false">
                <?php flynns_arcade_neon_icon('menu'); ?>
            </button>

            <?php flynns_arcade_neon_nav(); ?>
        </div>
    </header>
