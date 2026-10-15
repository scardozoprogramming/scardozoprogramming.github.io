$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.IO.Compression.FileSystem
$themeDirectory = Join-Path $PSScriptRoot '..\flynns-arcade-neon'
$themeDirectory = [System.IO.Path]::GetFullPath($themeDirectory)
$catalog = Get-Content -LiteralPath (Join-Path $themeDirectory 'inc\catalog.json') -Raw | ConvertFrom-Json
$editorial = Get-Content -LiteralPath (Join-Path $themeDirectory 'inc\editorial.json') -Raw | ConvertFrom-Json
$structure = Get-Content -LiteralPath (Join-Path $themeDirectory 'inc\structure.json') -Raw | ConvertFrom-Json
$downloadDirectory = Join-Path $themeDirectory 'downloads'
New-Item -ItemType Directory -Force -Path $downloadDirectory | Out-Null
$kits = @(
    @{ Slug = 'what-comes-after'; Title = 'What Comes After'; Index = 0 },
    @{ Slug = 'lambs-on-the-road'; Title = 'Lambs on the Road'; Index = 1 },
    @{ Slug = 'kolumno'; Title = 'Kolumno'; Index = 2 },
    @{ Slug = 'flynns-arcade'; Title = "Flynn's Arcade"; Index = 3 }
)
foreach ($kit in $kits) {
    $target = Join-Path $downloadDirectory ('presskit-' + $kit.Slug + '.zip')
    $stream = [System.IO.File]::Open($target, [System.IO.FileMode]::Create)
    $archive = [System.IO.Compression.ZipArchive]::new($stream, [System.IO.Compression.ZipArchiveMode]::Create)
    try {
        $game = $catalog | Where-Object { $_.title -eq $kit.Title } | Select-Object -First 1
        $info = @($kit.Title, '', "Publisher: Flynn's Arcade", ('Press contact: ' + $editorial.contact_email), '')
        if ($game) { $info += @($game.summary, '', ('Platforms listed: ' + ($game.platforms -join ', '))) }
        if ($kit.Slug -eq 'flynns-arcade') { $info += $editorial.about[0].paragraphs }
        $info += @('', 'Local media kit assembled from the public publisher website.', 'Includes the original publisher logo and supplied cover artwork.')
        $entry = $archive.CreateEntry('press-information.txt')
        $writer = [System.IO.StreamWriter]::new($entry.Open(), [System.Text.UTF8Encoding]::new($false))
        try { $writer.Write(($info -join [Environment]::NewLine)) } finally { $writer.Dispose() }
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($archive, (Join-Path $themeDirectory ('assets\images\' + $structure.logo)), 'flynns-arcade-original-logo.png') | Out-Null
        $cover = $structure.presskits[$kit.Index].image
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($archive, (Join-Path $themeDirectory ('assets\images\' + $cover)), ('cover' + [System.IO.Path]::GetExtension($cover))) | Out-Null
    } finally { $archive.Dispose(); $stream.Dispose() }
}
Write-Output 'Four local media kits generated.'
