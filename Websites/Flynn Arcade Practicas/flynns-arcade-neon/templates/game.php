<?php
if (!defined('ABSPATH')) { exit; }
$game = flynns_arcade_neon_current_game();
?>
<div class="section-inner game-page"><a class="text-link back-link" href="<?php echo esc_url(flynns_arcade_neon_url('games') . '#games'); ?>"><?php flynns_arcade_neon_icon('arrow-left'); ?>All games</a>
    <div class="game-detail-layout"><img class="game-detail-image" src="<?php echo esc_url(flynns_arcade_neon_asset($game['image'])); ?>" alt="<?php echo esc_attr($game['title']); ?>"><div class="game-detail-copy"><p class="eyebrow">Flynn's Arcade Publishing</p><h1><?php echo esc_html($game['title']); ?></h1><div class="tag-row"><?php foreach ($game['tags'] as $tag) : ?><span class="tag"><?php echo esc_html($tag); ?></span><?php endforeach; ?></div><p><?php echo esc_html($game['summary']); ?></p><h2>Platforms</h2><div class="game-platform-list"><?php foreach ($game['platforms'] as $platform) : ?><span><?php flynns_arcade_neon_platform_icon($platform); ?><span><?php echo esc_html($platform); ?></span></span><?php endforeach; ?></div></div></div>
</div>
<section class="section-inner other-games"><div class="section-heading"><h2 class="section-title">Other Games</h2></div><div class="games-grid"><?php $other_count = 0; foreach ($games as $other) : if ($other['title'] === $game['title']) continue; flynns_arcade_neon_game_card($other); if (++$other_count === 3) break; endforeach; ?></div></section>
