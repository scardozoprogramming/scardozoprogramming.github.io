<?php
defined('ABSPATH') || exit;
?>
<div id="main-content" role="region" aria-label="Flynn's Arcade content">
    <section class="hero" aria-label="Flynn's Arcade">
    <img class="hero-image" src="<?php echo esc_url(fae_asset_url('HOME/Images/hero-arcade-banner.webp')); ?>" alt="Panoramic neon arcade with a boy overlooking the city, surrounded by arcade machines" fetchpriority="high" width="2172" height="724">
</section>
<section class="featured-game" aria-labelledby="featured-title">
    <div class="section-inner featured-game-inner"><div class="featured-game-copy">
        <p class="eyebrow">Flynn's Arcade Publishing</p>
        <h1 id="featured-title"><span>The Last Cat</span><span>in the Universe</span></h1>
        <p class="featured-game-description">A short narrative incremental game about finding peace amidst the darkness.</p>
        <div class="featured-game-platform"><span class="platform-icon platform-switch" title="Switch"><svg role="img" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><title>Nintendo Switch</title><path d="M14.176 24h3.674c3.376 0 6.15-2.774 6.15-6.15V6.15C24 2.775 21.226 0 17.85 0H14.1c-.074 0-.15.074-.15.15v23.7c-.001.076.075.15.226.15zm4.574-13.199c1.351 0 2.399 1.125 2.399 2.398 0 1.352-1.125 2.4-2.399 2.4-1.35 0-2.4-1.049-2.4-2.4-.075-1.349 1.05-2.398 2.4-2.398zM11.4 0H6.15C2.775 0 0 2.775 0 6.15v11.7C0 21.226 2.775 24 6.15 24h5.25c.074 0 .15-.074.15-.149V.15c.001-.076-.075-.15-.15-.15zM9.676 22.051H6.15c-2.326 0-4.201-1.875-4.201-4.201V6.15c0-2.326 1.875-4.201 4.201-4.201H9.6l.076 20.102zM3.75 7.199c0 1.275.975 2.25 2.25 2.25s2.25-.975 2.25-2.25c0-1.273-.975-2.25-2.25-2.25s-2.25.977-2.25 2.25z"></path></svg><span class="screen-reader-text">Switch</span></span><span>Nintendo Switch</span></div>
        <div class="featured-actions"><a class="arcade-button" href="<?php echo esc_url(fae_game_url('the-last-cat-in-the-universe')); ?>">Discover the game <svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-arrow-right"></use></svg></a></div>
    </div>
    <a class="featured-game-link" href="<?php echo esc_url(fae_game_url('the-last-cat-in-the-universe')); ?>" aria-label="Discover The Last Cat in the Universe"><img class="featured-game-image" src="<?php echo esc_url(fae_asset_url('GAMES/Juegos/the-last-cat-in-the-universe/Images/cover.jpg')); ?>" data-game-preview="<?php echo esc_url(fae_asset_url('GAMES/Juegos/the-last-cat-in-the-universe/Images/preview.webp')); ?>" alt="The Last Cat in the Universe" width="640" height="360" decoding="async"></a>    </div>
</section>
    <section class="promotions" aria-label="Game highlights" aria-roledescription="carousel" data-promotions="">
        <div class="section-inner promotion-inner">
            <div class="promotion-slides">
                                    <article class="promotion-slide" data-promotion-slide="" aria-label="1 of 3">
                        <div class="promotion-copy"><p class="eyebrow">Eternum Ex</p><h2>Hell comes to Asia</h2><p class="promotion-description">Eternum Ex goes on sale in Asia on August 5.</p><a class="arcade-button" href="<?php echo esc_url(fae_game_url('eternum-ex')); ?>">Discover the game <svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-arrow-right"></use></svg></a></div>
                        <a class="promotion-visual promotion-visual-eternum" href="<?php echo esc_url(fae_game_url('eternum-ex')); ?>" aria-label="Discover Eternum Ex"><img class="promotion-art" src="<?php echo esc_url(fae_asset_url('GAMES/Juegos/eternum-ex/Images/promotion.png')); ?>" data-game-preview="<?php echo esc_url(fae_asset_url('GAMES/Juegos/eternum-ex/Images/preview.webp')); ?>" alt="Eternum Ex" loading="lazy" decoding="async" width="1600" height="800"></a>
                    </article>
                                    <article class="promotion-slide" data-promotion-slide="" hidden="" aria-label="2 of 3">
                        <div class="promotion-copy"><p class="eyebrow">Lambs on the Road</p><h2>When hope is the last thing to be lost</h2><p class="promotion-description">A platform adventure centered on holding onto hope.</p><a class="arcade-button" href="<?php echo esc_url(fae_game_url('lambs-on-the-road')); ?>">Discover the game <svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-arrow-right"></use></svg></a></div>
                        <a class="promotion-visual" href="<?php echo esc_url(fae_game_url('lambs-on-the-road')); ?>" aria-label="Discover Lambs on the Road"><img class="promotion-art" src="<?php echo esc_url(fae_asset_url('GAMES/Juegos/lambs-on-the-road/Images/cover.jpg')); ?>" data-game-preview="<?php echo esc_url(fae_asset_url('GAMES/Juegos/lambs-on-the-road/Images/preview.webp')); ?>" alt="Lambs on the Road" loading="lazy" decoding="async" width="600" height="450"></a>
                    </article>
                                    <article class="promotion-slide" data-promotion-slide="" hidden="" aria-label="3 of 3">
                        <div class="promotion-copy"><p class="eyebrow">Saga</p><h2>A Viking girl's quest for revenge.</h2><p class="promotion-description">An action platformer from Flynn's Arcade.</p><a class="arcade-button" href="https://store.steampowered.com/app/2590820/Saga/?l=english" target="_blank" rel="noopener noreferrer">Discover the game <svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-arrow-right"></use></svg></a></div>
                        <a class="promotion-visual" href="https://store.steampowered.com/app/2590820/Saga/?l=english" target="_blank" rel="noopener noreferrer" aria-label="View Saga on Steam"><img class="promotion-art" src="<?php echo esc_url(fae_asset_url('ABOUT/Images/project-saga.webp')); ?>" alt="Saga" loading="lazy" decoding="async" width="616" height="353"></a>
                    </article>
                            </div>
            <div class="promotion-controls"><button class="icon-button" type="button" data-promotion-step="-1" aria-label="Previous highlight" title="Previous highlight"><svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-chevron-left"></use></svg></button><div class="promotion-dots"><button type="button" class="promotion-dot" data-promotion-index="0" aria-label="Eternum Ex" aria-pressed="true"></button><button type="button" class="promotion-dot" data-promotion-index="1" aria-label="Lambs on the Road" aria-pressed="false"></button><button type="button" class="promotion-dot" data-promotion-index="2" aria-label="Saga" aria-pressed="false"></button></div><button class="icon-button" type="button" data-promotion-step="1" aria-label="Next highlight" title="Next highlight"><svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-chevron-right"></use></svg></button></div>
        </div>
    </section>
    <?php fae_template_part('GAMES/PHP/catalog', null, array('show_heading' => true)); ?>
</div>
