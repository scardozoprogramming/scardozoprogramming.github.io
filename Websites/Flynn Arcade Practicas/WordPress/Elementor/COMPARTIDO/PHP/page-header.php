<?php
defined('ABSPATH') || exit;
?>
<a class="skip-link screen-reader-text" href="#main-content">Skip to content</a>
<header class="site-header"><div class="header-inner">
    <a class="brand" href="<?php echo esc_url(fae_page_url('home')); ?>" aria-label="Flynn's Arcade home"><span class="publisher-logo"><img src="<?php echo esc_url(fae_asset_url('COMPARTIDO/Images/Brand/flynns-publishing-pixel.png')); ?>" width="1600" height="1000" alt="Flynn's Arcade Publishing" decoding="async"></span></a>
    <nav class="site-nav" id="site-navigation" aria-label="Primary">
        <?php foreach (fae_sections() as $key => $title) : ?>
        <a href="<?php echo esc_url(fae_page_url($key)); ?>"<?php if ($key === $args['section']) { echo ' aria-current="page"'; } ?>><?php echo esc_html($title); ?></a>
        <?php endforeach; ?>
    </nav>
    <?php fae_template_part('COMPARTIDO/PHP/header-socials'); ?>
    <button class="nav-toggle icon-button" type="button" aria-label="Open navigation" aria-controls="site-navigation" aria-expanded="false"><svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-menu"></use></svg></button>
</div></header>
