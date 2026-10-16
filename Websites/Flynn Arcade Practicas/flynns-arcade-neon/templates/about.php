<?php if (!defined('ABSPATH')) { exit; } ?>
<h1 class="screen-reader-text">About</h1>
<div class="about-brand"><?php flynns_arcade_neon_logo(); ?></div>
<section class="about-gallery" aria-label="Studio photos" data-gallery>
    <?php foreach ($structure['about_gallery'] as $photo) : ?>
        <a href="<?php echo esc_url(flynns_arcade_neon_asset($photo['image'])); ?>" data-gallery-item title="View photo"><img src="<?php echo esc_url(flynns_arcade_neon_asset($photo['image'])); ?>" alt="<?php echo esc_attr($photo['alt']); ?>" loading="lazy" decoding="async" width="1200" height="800"></a>
    <?php endforeach; ?>
</section>
<div class="about-story">
    <?php foreach ($editorial['about'] as $section) : ?>
        <section><h2><?php echo esc_html($section['title']); ?></h2><div class="about-story-copy"><?php foreach ($section['paragraphs'] as $paragraph) : ?><p><?php echo esc_html($paragraph); ?></p><?php endforeach; ?></div></section>
    <?php endforeach; ?>
</div>
<section class="about-projects section-inner" aria-labelledby="projects-title">
    <div class="section-heading"><h2 class="section-title" id="projects-title">Projects</h2></div>
    <div class="projects-grid">
        <?php foreach ($structure['about_projects'] as $project) : ?>
            <?php foreach ($games as $game) : ?>
                <?php if (flynns_arcade_neon_slug($game['title']) !== $project['slug']) continue; ?>
                <?php flynns_arcade_neon_game_card($game); break; ?>
            <?php endforeach; ?>
        <?php endforeach; ?>
    </div>
</section>
<dialog class="image-viewer" data-gallery-dialog aria-label="Studio photo gallery">
    <div class="image-viewer-toolbar"><p data-gallery-count aria-live="polite"></p><button class="icon-button" type="button" data-gallery-close aria-label="Close photo" title="Close photo"><?php flynns_arcade_neon_icon('x'); ?></button></div>
    <div class="image-viewer-stage"><button class="icon-button" type="button" data-gallery-step="-1" aria-label="Previous photo" title="Previous photo"><?php flynns_arcade_neon_icon('chevron-left'); ?></button><img data-gallery-image alt=""><button class="icon-button" type="button" data-gallery-step="1" aria-label="Next photo" title="Next photo"><?php flynns_arcade_neon_icon('chevron-right'); ?></button></div>
</dialog>
