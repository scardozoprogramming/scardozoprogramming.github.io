<?php
/** Site-wide header. @package FlynnsArcadeNeon */
if (!defined('ABSPATH')) { exit; }
?>
<!doctype html>
<html <?php language_attributes(); ?>>
<head><meta charset="<?php bloginfo('charset'); ?>"><meta name="viewport" content="width=device-width, initial-scale=1"><?php wp_head(); ?></head>
<body <?php body_class(); ?> data-view="<?php echo esc_attr(flynns_arcade_neon_view()); ?>">
<?php wp_body_open(); ?>
<?php readfile(get_template_directory() . '/assets/icons/ui.svg'); ?>
<a class="skip-link screen-reader-text" href="#main-content">Skip to content</a>
<header class="site-header"><div class="header-inner">
    <?php flynns_arcade_neon_brand(); ?>
    <?php flynns_arcade_neon_nav(); ?>
    <div class="header-socials"><?php flynns_arcade_neon_socials(true); ?></div>
    <button class="nav-toggle icon-button" type="button" aria-label="Open navigation" aria-controls="site-navigation" aria-expanded="false"><?php flynns_arcade_neon_icon('menu'); ?></button>
</div></header>
