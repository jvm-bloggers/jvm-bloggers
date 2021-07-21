{{/*
Expand the name of the chart.
*/}}
{{- define "jvm-bloggers.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "jvm-bloggers.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "jvm-bloggers.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "jvm-bloggers.labels" -}}
helm.sh/chart: {{ include "jvm-bloggers.chart" . }}
{{ include "jvm-bloggers.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "jvm-bloggers.selectorLabels" -}}
app.kubernetes.io/name: {{ include "jvm-bloggers.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "jvm-bloggers.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "jvm-bloggers.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{- define "jasyptEncryptorConfigSource" -}}
{{- printf "%s%s" .Values.jasypt.encryptor.kind "KeyRef" -}}
{{- end -}}

{{- define "dbHostConfigSource" -}}
{{- printf "%s%s" .Values.db.hostConfig.kind "KeyRef" -}}
{{- end -}}

{{- define "dbPortConfigSource" -}}
{{- printf "%s%s" .Values.db.portConfig.kind "KeyRef" -}}
{{- end -}}

{{- define "dbNameConfigSource" -}}
{{- printf "%s%s" .Values.db.nameConfig.kind "KeyRef" -}}
{{- end -}}

{{- define "dbUsernameConfigSource" -}}
{{- printf "%s%s" .Values.db.usernameConfig.kind "KeyRef" -}}
{{- end -}}

{{- define "dbPasswordConfigSource" -}}
{{- printf "%s%s" .Values.db.passwordConfig.kind "KeyRef" -}}
{{- end -}}