<?php
/* Template Name: Flynn's Arcade: Indies */
defined('ABSPATH') || exit;
get_header();
?>
<main id="main-content">
    <div class="page-heading section-inner"><p class="eyebrow">Independent creators</p><h1>We Love <span>Indies.</span></h1></div>
<div class="section-inner studio-directory" data-directory="studios" data-directory-page-size="7">
<div class="directory-toolbar" data-directory-tools="" hidden="">
        <div class="catalog-search"><label class="search-wrap"><svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-search"></use></svg><span class="screen-reader-text">Search studios or games</span><input type="search" data-directory-search="" placeholder="Search studios or games" autocomplete="off" maxlength="100"></label><button class="icon-button" type="button" data-directory-clear="" aria-label="Clear search" title="Clear search" hidden=""><svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-x"></use></svg></button></div>
        <p class="directory-count" data-directory-count="" role="status"></p>
    </div>
<?php get_template_part('INDIES/Estudios/radin-games/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/fahmitsu-rolling-glory-jam/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/pablo-navarro-rawrlab-games/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/jose-catcade-games/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/jeremy-fryc-stardust-team/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/sebastian-pixel-games/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/xavi-hiulit/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/soufiane-tacsou/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/andrea-der-mondhase/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/andrea-tarondo-silver978/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/joel-potter-studio-primitive/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/david-langlois-vidvad-games/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/may-oswald-maybell-soft/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/luke-orsie-crashable-studios/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/enzo-brown-slain-mascot/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/magnus-fredriksson-bortbyting/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/apamate-studio/PHP/studio'); ?>
<?php get_template_part('INDIES/Estudios/dust-games/PHP/studio'); ?>
<p class="empty-state" data-directory-empty="" hidden="">No results found.</p>
<div class="load-more-row"><button class="arcade-button" type="button" data-directory-more="" hidden="">More studios<svg class="icon" aria-hidden="true" focusable="false"><use href="#icon-plus"></use></svg></button></div>
</div>
</main>
<?php get_footer(); ?>
