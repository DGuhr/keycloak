$reportPath = 'target'
$archive = "${reportPath}\reports-quarkus-tests-windows.zip"

$files = Get-ChildItem -Path .\ -Directory -Recurse -Include 'surefire-reports' |
              Foreach-Object {Get-ChildItem $_ -File -Filter 'report.xml'}

$fullFilenames = $files | ForEach-Object -Process {Write-Output -InputObject $_.FullName}

# Create archive
if (Test-Path $archive) { Remove-Item $archive -ErrorAction Stop }
Add-Type -AssemblyName System.IO.Compression
Add-Type -AssemblyName System.IO.Compression.FileSystem
$zip = [System.IO.Compression.ZipFile]::Open(($archive), [System.IO.Compression.ZipArchiveMode]::Create)

foreach ($fname in $fullFilenames) {
    $rname = $(Resolve-Path -Path $fname -Relative) -replace '^\.\\',''
    Write-Output $rname
    $zentry = $zip.CreateEntry($rname)
    $zentryWriter = New-Object -TypeName System.IO.BinaryWriter $zentry.Open()
    $zentryWriter.Write([System.IO.File]::ReadAllBytes($fname))
    $zentryWriter.Flush()
    $zentryWriter.Close()
}

# Clean up
$zip.Dispose()