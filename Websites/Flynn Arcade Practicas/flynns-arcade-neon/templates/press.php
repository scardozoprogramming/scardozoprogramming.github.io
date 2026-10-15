<?php if (!defined('ABSPATH')) { exit; } ?>
<div class="page-heading section-inner"><p class="eyebrow">Company news highlights and presskits</p><h1>Press <span>Media.</span></h1></div>
<div class="section-inner press-directory">
    <?php foreach ($editorial['press'] as $index => $item) : $review = $structure['press'][$index]; ?>
        <article class="press-row"><img class="press-image" src="<?php echo esc_url(flynns_arcade_neon_asset($review['image'])); ?>" alt="<?php echo esc_attr($item['title']); ?>" loading="lazy"><div class="press-copy"><p class="eyebrow"><?php echo esc_html($item['date'] . ' / ' . $item['outlet']); ?></p><h2><?php echo esc_html($review['heading']); ?></h2><p><?php echo esc_html($site['press_summaries'][$index]); ?></p><a class="text-link" href="<?php echo esc_url($review['article_url']); ?>" target="_blank" rel="noopener noreferrer">Read more <?php flynns_arcade_neon_icon('external-link'); ?></a></div></article>
    <?php endforeach; ?>
</div>
<section class="section-inner presskits"><div class="section-heading"><h2 class="section-title">Check out the presskits</h2></div><div class="presskit-grid">
    <?php foreach (array('what-comes-after' => 'What Comes After', 'lambs-on-the-road' => 'Lambs on the Road', 'kolumno' => 'Kolumno', 'flynns-arcade' => "Flynn's Arcade") as $slug => $name) : $index = array_search($slug, array('what-comes-after', 'lambs-on-the-road', 'kolumno', 'flynns-arcade'), true); ?>
        <a class="presskit" href="<?php echo esc_url(get_theme_file_uri('downloads/presskit-' . $slug . '.zip')); ?>" download><img src="<?php echo esc_url(flynns_arcade_neon_asset($structure['presskits'][$index]['image'])); ?>" alt="<?php echo esc_attr($name); ?>" loading="lazy"><span><strong><?php echo esc_html($name); ?></strong><span>Presskit <?php flynns_arcade_neon_icon('download'); ?></span></span></a>
    <?php endforeach; ?>
</div></section>
<?php flynns_arcade_neon_promotions(); ?>
