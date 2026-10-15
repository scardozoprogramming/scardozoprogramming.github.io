<?php if (!defined('ABSPATH')) { exit; } ?>
<div class="page-heading section-inner"><p class="eyebrow">Independent creators</p><h1>We Love <span>Indies.</span></h1></div>
<div class="section-inner studio-directory">
    <?php foreach ($editorial['studios'] as $index => $studio) : $media = $structure['studios'][$index]; ?>
        <article class="studio-row" aria-labelledby="studio-<?php echo $index; ?>">
            <div class="studio-identity"><p class="studio-games"><?php echo esc_html($studio['detail']); ?></p><h2 id="studio-<?php echo $index; ?>"><?php echo esc_html($studio['name']); ?></h2><img class="studio-logo" src="<?php echo esc_url(flynns_arcade_neon_asset($media['logo'])); ?>" alt="<?php echo esc_attr($studio['name'] . ' logo'); ?>" loading="lazy"></div>
            <img class="studio-portrait" src="<?php echo esc_url(flynns_arcade_neon_asset($media['portrait'])); ?>" alt="<?php echo esc_attr($studio['name']); ?>" loading="lazy">
            <div class="studio-description"><p><?php echo esc_html($site['studio_bios'][$index]); ?></p><?php if (!empty($media['website'])) : ?><a class="text-link studio-website" href="<?php echo esc_url($media['website']); ?>" target="_blank" rel="noopener noreferrer"><?php echo esc_html(parse_url($media['website'], PHP_URL_HOST)); ?><?php flynns_arcade_neon_icon('external-link'); ?></a><?php endif; ?></div>
        </article>
    <?php endforeach; ?>
</div>
