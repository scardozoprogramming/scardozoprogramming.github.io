<?php
defined('ABSPATH') || exit;
$flynns_context = flynns_context();
?>
<!doctype html>
<html <?php language_attributes(); ?>>
<head>
    <meta charset="<?php bloginfo('charset'); ?>">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <?php if (!has_site_icon()) : ?>
    <link rel="icon" href="<?php echo esc_url(flynns_asset_url('COMPARTIDO/Images/Brand/favicon-original.png')); ?>">
    <?php endif; ?>
    <?php wp_head(); ?>
</head>
<body <?php body_class(); ?> data-view="<?php echo esc_attr($flynns_context['type']); ?>" data-page="<?php echo esc_attr($flynns_context['type']); ?>">
<?php wp_body_open(); ?>
<?php get_template_part('COMPARTIDO/PHP/icons'); ?>
<a class="skip-link screen-reader-text" href="#main-content">Skip to content</a>
<header class="site-header"><div class="header-inner">
    <a class="brand" href="<?php echo esc_url(flynns_page_url('home')); ?>" aria-label="Flynn's Arcade home"><span class="publisher-logo"><img src="<?php echo esc_url(flynns_asset_url('COMPARTIDO/Images/Brand/flynns-publishing-pixel.png')); ?>" width="1600" height="1000" alt="Flynn's Arcade Publishing" decoding="async"></span></a>
    <nav class="site-nav" id="site-navigation" aria-label="Primary">
        <?php foreach (flynns_sections() as $key => $definition) : ?>
        <a href="<?php echo esc_url(flynns_page_url($key)); ?>"<?php if ($key === $flynns_context['section']) { echo ' aria-current="page"'; } ?>><?php echo esc_html($definition['title']); ?></a>
        <?php endforeach; ?>
    </nav>
    <?php get_template_part('COMPARTIDO/PHP/header-socials'); ?>
    <button class="nav-toggle icon-button" type="button" aria-label="Open navigation" aria-controls="site-navigation" aria-expanded="false"><svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-menu"></use></svg></button>
</div></header>
